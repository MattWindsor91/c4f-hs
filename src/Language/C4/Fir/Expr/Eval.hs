--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Expr.Op
-- Description : C4 Fuzzable Internal Representation: Operators
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
  ( MonadEval (load, store, rmw, cmpxchg)
  , evalExpr
    -- Error
  , EvalError (VarError)
  ) where

import qualified Data.Functor.Foldable as F
import Control.Applicative (liftA2)
import Data.Bits (complement)
import Data.Function (on)
import Language.C4.Fir.Const (Const (..), bool, i32, coerceBool, coerceI32)
import Language.C4.Fir.Expr.Expr (Expr (..), ExprF (..), PrimExpr (..) )
import qualified Language.C4.Fir.Expr.Op as Op
import Language.C4.Fir.Lvalue (NormAddress, normalise)
import Language.C4.Fir.Atomic.MemOrder (MemOrder (..))

-- | Monad capturing enough of the memory model to evaluate expressions.
class Monad m => MonadEval m where
  -- | Loads a heap value, potentially atomically.
  load :: Maybe MemOrder -> NormAddress -> m Const
  -- | Stores a heap value, potentially atomically.
  store :: Maybe MemOrder -> NormAddress -> Const -> m ()
  -- | Atomically reads, modifies, and writes a heap value.
  rmw :: MemOrder -> NormAddress -> (Const -> Const) -> m Const
  {- | Compare and exchange: `cmpxchg succ fail obj expected desired` has
       the same semantics as `atomic_compare_exchange_strong_explicit`. -}
  cmpxchg :: MemOrder -> MemOrder -> NormAddress -> NormAddress -> Const -> m Bool


-- | Evaluates an expression in an evaluation monad.
evalExpr :: MonadEval m => Expr a -> m Const
{- We use a recursion-schemes fold, whereby at each step of evaluation we have
   reduced any sub-term into `m Const`: in other words, the raw computation
   that will result in a constant value if we evaluate it.  (We can't evaluate
   to a `Const` in a recursion because of short-circuiting in some operators.)
-}
evalExpr = F.fold evalExpr'
  -- Evaluation ignores metadata.
  -- Every other leg delegates to several sub-evaluators we define below.
  where evalExpr' (MetaF _ k  ) = k
        evalExpr' (PrimF p    ) = evalPrim p
        evalExpr' (BinF  o l r) = evalBop o l r
        evalExpr' (UnF   o x  ) = evalUop o x

-- | Evaluates a primitive expression.
evalPrim :: MonadEval m => PrimExpr -> m Const
-- Evaluating constants is trivial: we just return them.
evalPrim (Con x) = pure x
-- To evaluate addresses, we normalise them, then look them up in the current
-- heap with non-atomic semantics.
evalPrim (Addr a) = load Nothing (normalise a)

-- | Provides the main shape of a binary operator evaluator.
--
-- Arithmetic and bitwise operators map, purely, from integers to integers,
-- so they share most of their boilerplate.
evalBopGen
  :: MonadEval m
  => (Const -> a)             -- ^ A lifter for argument constants.
  -> (b -> Const)             -- ^ An unlifter for the final value.
  -> (m a -> m a -> m b)      -- ^ The semantics of the operator.
  -> m Const                  -- ^ The left argument.
  -> m Const                  -- ^ The right argument.
  -> m Const                  -- ^ The final result.
evalBopGen arg res f = evalBopGen' `on` (arg <$>)
  where evalBopGen' l r = res <$> f l r

-- | Evaluates an arithmetic binary operation.
evalABop :: MonadEval m => Op.ABop -> m Const -> m Const -> m Const
evalABop = evalBopGen coerceI32 i32 . liftA2 . Op.semABop

-- | Evaluates a bitwise binary operation.
evalBBop :: MonadEval m => Op.BBop -> m Const -> m Const -> m Const
evalBBop = evalBopGen coerceI32 i32 . liftA2 . Op.semBBop


-- | Evaluates a logical binary operation.
--
-- Logical operations map from Booleans to Booleans, but carry through the
-- monad, so as to allow for short-circuit evaluation.
evalLBop :: MonadEval m => Op.LBop -> m Const -> m Const -> m Const
evalLBop = evalBopGen coerceBool bool . Op.semLBop

{- Relational operations need a degree of agreement as to what the type is.
   For now, we just coerce both sides to Int32, as it forms a strict extension
   of booleans; if we get multiple bit widths, we may need to rethink this. -}

-- | Evaluates a relational binary operation.
evalRBop :: MonadEval m => Op.RBop -> m Const -> m Const -> m Const
evalRBop = evalBopGen coerceI32 bool . liftA2 . Op.semRBop

-- | Evaluates a binary operator.
evalBop :: MonadEval m => Op.Bop -> m Const -> m Const -> m Const
evalBop (Op.Arith   o) = evalABop o
evalBop (Op.Bitwise o) = evalBBop o
evalBop (Op.Logical o) = evalLBop o
evalBop (Op.Rel     o) = evalRBop o

-- | Evaluates a unary operator.
evalUop :: MonadEval m => Op.Uop -> m Const -> m Const
evalUop Op.Comp = fmap (i32 . complement . coerceI32)
evalUop Op.Not  = fmap (bool . not . coerceBool)

-- | Enumeration of possible errors when evaluating an expression.
newtype EvalError
  = VarError NormAddress -- ^ A variable reference failed lookup in the heap.
    deriving (Eq, Show)
