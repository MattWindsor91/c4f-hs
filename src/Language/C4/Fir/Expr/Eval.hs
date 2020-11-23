{-# LANGUAGE NamedFieldPuns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Expr.Eval
-- Description : Operator definitions for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- The successful testing of various parts of the fuzzer system depends on the
-- ability to evaluate expressions (at least, in an idealised sequential version
-- of the FIR semantics).
--
-- This module defines a rough operational semantics FIR expressions
-- parametrised on the atomic semantics of various heap operations.
--------------------------------------------------------------------------------
--
module Language.C4.Fir.Expr.Eval
  ( evalExpr
    -- * The evaluation monad
  , MonadEval (load, store, rmw, cmpxchg, err)
    -- * Errors
  , EvalError (CoerceError, NotMoError, VarError)
  ) where

import qualified Data.Functor.Foldable as F
import Control.Applicative (liftA2)
import qualified Control.Lens as L
import Data.Bits (complement)
import Data.Function (on)
import Data.Maybe (fromJust)
import qualified Type.Reflection as TR
import qualified Language.C4.Fir.Atomic.Action as A
import qualified Language.C4.Fir.Const as K
import Language.C4.Fir.Expr.Expr
  ( Expr (..) 
  , ExprF (..)
  , CExpr (..)
  , PExpr (..)
  )
import qualified Language.C4.Fir.Expr.Op as Op
import qualified Language.C4.Fir.Type as T
import Language.C4.Fir.Lvalue (NormAddress, normalise)
import Language.C4.Fir.Atomic.MemOrder (MemOrder (..), MemOrderArg (..))

-- | Monad capturing enough of the memory model to evaluate expressions.
class Monad m => MonadEval m where
  -- | Loads a heap value, potentially atomically.
  load :: Maybe MemOrder -> NormAddress -> m K.Const
  -- | Stores a heap value, potentially atomically.
  store :: Maybe MemOrder -> NormAddress -> K.Const -> m ()
  -- | Atomically reads, modifies, and writes a heap value.
  rmw :: MemOrder -> NormAddress -> (K.Const -> K.Const) -> m K.Const
  -- | Compare and exchange: @cmpxchg succ fail obj expected desired@ has
  --   the same semantics as @atomic_compare_exchange_strong_explicit@.
  cmpxchg :: MemOrder -> MemOrder -> NormAddress -> NormAddress -> K.Const -> m Bool
  -- | Throws an error.
  err :: EvalError -> m a


-- | Evaluates an expression in an evaluation monad.
evalExpr
  :: MonadEval m
  => Expr a    -- ^ The expression to evaluate.
  -> m K.Const -- ^ The computation for the result.
{- We use a recursion-schemes fold, whereby at each step of evaluation we have
   reduced any sub-term into `m Const`: in other words, the raw computation
   that will result in a constant value if we evaluate it.  (We can't evaluate
   to a `Const` in a recursion because of short-circuiting in some operators.)
-}
evalExpr = F.fold evalExpr'
  -- Evaluation ignores metadata.
  -- Every other leg delegates to several sub-evaluators we define below.
  where evalExpr' (MetaF  _ k  ) = k
        evalExpr' (PrimF  p    ) = evalPExpr p
        evalExpr' (ALoadF l    ) = evalALoad l
        evalExpr' (CondF  c    ) = evalCExpr c
        evalExpr' (BinF   o l r) = evalBop o l r
        evalExpr' (UnF    o x  ) = evalUop o x

-- | Evaluates a primitive expression.
evalPExpr
  :: MonadEval m
  => PExpr       -- ^ The primitive expression.
  -> m K.Const   -- ^ The computation for the result.
-- Evaluating constants is trivial: we just return them.
evalPExpr (Con x) = pure x
-- To evaluate addresses, we normalise them, then look them up in the current
-- heap with non-atomic semantics.
evalPExpr (Addr a) = load Nothing (normalise a)

-- | Evaluates an atomic load whose memory order has been part-evaluated.
evalALoad
  :: MonadEval m
  => A.Load (m K.Const) -- ^ The atomic load.
  -> m K.Const          -- ^ The computation for the result.
evalALoad A.Load { A._src, A._mo } =
  moArg _mo >>= \mo -> load (Just mo) (normalise _src)

{-
 - Conditional expressions
 -}

-- | Evaluates a conditional expression whose legs have been part-evaluated.
evalCExpr
  :: MonadEval m
  => CExpr (m K.Const) -- ^ The conditional expression.
  -> m K.Const         -- ^ The computation for the result.
evalCExpr CExpr { _cond, _tBranch, _fBranch } =
  _cond >>= liftCoerce K.coerceBool >>= doCond
  where doCond c = if c then _tBranch else _fBranch

{-
 - Binary operators
 -}

-- | Provides the main shape of a binary operator evaluator.
evalBopGen
  :: (MonadEval m, TR.Typeable a)
  => (K.Const -> Maybe a)  -- ^ A lifter for argument constants.
  -> (b -> K.Const)        -- ^ An unlifter for the final value.
  -> (m a -> m a -> m b)   -- ^ The semantics of the operator.
  -> m K.Const             -- ^ The computation for the LHS operand.
  -> m K.Const             -- ^ The computation for the RHS operand.
  -> m K.Const             -- ^ The computation for the result.
evalBopGen arg res f = evalBopGen' `on` (>>= liftCoerce arg)
  where evalBopGen' l r = res <$> f l r

-- | Type signature of binary operation evaluators.
type BopEval o m
  =  o         -- ^ The arithmetic operator.
  -> m K.Const -- ^ The computation for the LHS operand.
  -> m K.Const -- ^ The computation for the RHS operand.
  -> m K.Const -- ^ The computation for the binary operation.

-- | Evaluates an arithmetic binary operation.
evalABop :: MonadEval m => BopEval Op.ABop m
evalABop = evalBopGen K.coerceI32 K.i32 . liftA2 . Op.semABop

-- | Evaluates a bitwise binary operation.
evalBBop :: MonadEval m => BopEval Op.BBop m
evalBBop = evalBopGen K.coerceI32 K.i32 . liftA2 . Op.semBBop

-- | Evaluates a logical binary operation.
--
-- Logical operations map from Booleans to Booleans, but carry through the
-- monad, so as to allow for short-circuit evaluation.
evalLBop :: MonadEval m => BopEval Op.LBop m
evalLBop = evalBopGen K.coerceBool K.bool . Op.semLBop

{- Relational operations need a degree of agreement as to what the type is.
   For now, we just coerce both sides to Int32, as it forms a strict extension
   of booleans; if we get multiple bit widths, we may need to rethink this. -}

-- | Evaluates a relational binary operation.
evalRBop :: MonadEval m => BopEval Op.RBop m
evalRBop = evalBopGen K.coerceI32 K.bool . liftA2 . Op.semRBop

-- | Evaluates a binary operator.
evalBop :: MonadEval m => BopEval Op.Bop m
evalBop (Op.Arith   o) = evalABop o
evalBop (Op.Bitwise o) = evalBBop o
evalBop (Op.Logical o) = evalLBop o
evalBop (Op.Rel     o) = evalRBop o

{-
 - Unary operators
 -}

-- | Provides the main shape of a unary operator evaluator.
evalUopGen
  :: (MonadEval m, TR.Typeable a)
  => (K.Const -> Maybe a)  -- ^ A lifter for argument constants.
  -> (b -> K.Const)        -- ^ An unlifter for the final value.
  -> (m a -> m b)          -- ^ The semantics of the operator.
  -> m K.Const             -- ^ The computation for the operand.
  -> m K.Const             -- ^ The computation for the result.
evalUopGen arg res f = fmap res . f . (>>= liftCoerce arg)

-- | Evaluates a unary operator.
evalUop :: MonadEval m => Op.Uop -> m K.Const -> m K.Const
evalUop Op.Comp = evalUopGen K.coerceI32  K.i32  (fmap complement)
evalUop Op.Not  = evalUopGen K.coerceBool K.bool (fmap not)

{-
 - Memory orders
 -}

-- | Evaluates a (partially evaluated) memory order argument to a memory order.
--
-- Per usual C11 semantics, implicit memory orders expand to SC.
moArg
  :: MonadEval m
  => MemOrderArg (m K.Const) -- ^ The partially-evaluated memory order argument.
  -> m MemOrder              -- ^ The final memory order.
moArg Implicit     = return SeqCst
moArg (Explicit e) = asMo =<< e

-- | Interprets a constant as a memory order.
asMo
  :: MonadEval m
  => K.Const     -- ^ The constant to interpret.
  -> m MemOrder  -- ^ The final memory order.
-- This could be a variant of liftCoerce/coerceXYZ, but we'd need to deal with
-- the reflection back into types; MemOrder doesn't have one yet.
asMo = either moErr return . L.matching K._Mo
  where moErr k = err (NotMoError k)

{-
 - Errors and miscellanea
 -}

-- | Lifts a coersion into the evaluation monad.
liftCoerce
  :: (MonadEval m, TR.Typeable a)
  => (K.Const -> Maybe a) -- ^ The coersion.
  -> K.Const              -- ^ The constant to coerce.
  -> m a                  -- ^ The final result.
liftCoerce f k = addTypeError (f k) k

-- | Tags a coersion with a type error.
--
--   This separate function mainly exists to make the use of Typeable behave.
addTypeError
  :: (MonadEval m, TR.Typeable a)
  => Maybe a -- ^ The result of the coersion.
  -> K.Const -- ^ The constant that was coerced.
  -> m a     -- ^ The final result.
addTypeError r k = maybe (err (error r)) return r
  -- The fromJust here is literally here to get the right type for Typeable;
  -- it doesn't actually get evaluated.
  where error = CoerceError k . T.liftHsType . TR.typeOf . fromJust

-- | Enumeration of possible errors when evaluating an expression.
data EvalError
  = VarError NormAddress
    -- ^ A variable reference failed lookup in the heap.
  | CoerceError K.Const (Maybe T.SType)
    -- ^ A coersion failed.
  | NotMoError K.Const
    -- ^ We expected an expression to evaluate to a memory order, but it didn't.
    deriving (Eq, Show)
