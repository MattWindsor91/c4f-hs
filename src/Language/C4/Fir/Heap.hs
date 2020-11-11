{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Heap
-- Description : Basic sequential heap model
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------
module Language.C4.Fir.Heap
  ( Heap
  , empty
  , get
  , update
    -- * The sequential evaluation monad
  , SeqEval
  , seqLoad
  , seqStore 
  , seqRmw
  , seqCmpxchg
    -- * Evaluating expressions against a heap
  , seqEvalExpr'
  , seqEvalExpr
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, gets, modify, runStateT, evalStateT)
import Data.Function ((&))
import qualified Language.C4.Fir.Const as K
import qualified Language.C4.Fir.Expr.Eval as E
import qualified Language.C4.Fir.Expr.Expr as X
import qualified Language.C4.Fir.Lvalue as L

-- | A shallow heap model.
type Heap = L.NormAddress -> Maybe K.Const

-- | The empty heap.
empty :: Heap
empty = const Nothing

{- We update a heap by producing a new function that checks the new mapping
   first, and then delegates to the old heap.  This is quite inefficient, but
   at time of writing we don't expect the Seq heap model to be used very
   extensively.  -}

-- | Updates a heap with a new mapping.
update :: L.NormAddress -> K.Const -> Heap -> Heap
update a k h a' = if a == a' then Just k else h a'

-- | Looks up an address in the heap.
get :: L.NormAddress -> Heap -> Maybe K.Const
get = (&)

{-
 - The sequential evaluation monad
 -}

-- | Model of expression evaluation with sequential semantics.
type SeqEval = StateT Heap (Either E.EvalError)

{- The monad implements MonadEval, but does so by ignoring memory orders and
   propagating heap changes immediately. -}
instance E.MonadEval SeqEval where
  load _ = seqLoad
  store _ = seqStore
  rmw _ = seqRmw
  cmpxchg _ _ = seqCmpxchg

-- | Loads an address in the Seq evaluation model.
seqLoad :: L.NormAddress -> SeqEval K.Const
seqLoad a = gets (get a) >>= seqTag (E.VarError a)

-- | Lifts a partial evaluation into the `SeqEval` monad by tagging failure to
--   evaluate with an error.
seqTag :: E.EvalError -> Maybe a -> SeqEval a
seqTag e = maybe (throwError e) return

-- | Stores a constant to an address in the Seq evaluation model.
seqStore :: L.NormAddress -> K.Const -> SeqEval ()
seqStore a = modify . update a

{- We can define the various read-modify-write actions through loads and stores,
   in a sequential heap. -}

-- | Read-modify-writes a constant at an address in the Seq evaluation model.
seqRmw :: L.NormAddress -> (K.Const -> K.Const) -> SeqEval K.Const
seqRmw a f = seqLoad a >>= modWrite
  where modWrite v = v <$ seqStore a (f v)

-- | Performs a C-style compare-exchange in the Seq evaluation model.
seqCmpxchg :: L.NormAddress -> L.NormAddress -> K.Const -> SeqEval Bool
seqCmpxchg o e d =
  do
    ov <- seqLoad o
    ev <- seqLoad e
    if ov == ev
    then True <$ seqStore o d
    else False <$ seqStore e ov

{-
 - Evaluating expressions against a heap
 -}

-- | Evaluates an expression in a sequential heap,
--   returning the new heap and constant final value on success.
seqEvalExpr' :: X.Expr a -> Heap -> Either E.EvalError (K.Const, Heap)
seqEvalExpr' = runStateT . E.evalExpr

-- | Evaluates an expression in a sequential heap,
--   returning the constant final value on success.
seqEvalExpr :: X.Expr a -> Heap -> Either E.EvalError K.Const
seqEvalExpr = evalStateT . E.evalExpr
