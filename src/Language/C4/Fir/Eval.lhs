Evaluating Fir expressions
==========================

The successful testing of various parts of the fuzzer system depends on the
ability to evaluate expressions (at least, in an idealised sequential version
of the Fir semantics).

> {-# LANGUAGE
>   TypeSynonymInstances
> , FlexibleInstances
> #-}

> {-|
> Module      : Language.C4.Fir.Eval
> Description : C4 Fuzzable Internal Representation: Expression evaluation
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.Eval
>   ( MonadEval (load, store, rmw, cmpxchg)
>   , evalExpr
>   , seqEvalExpr
>   , seqEvalExpr'
>     -- Heaps
>   , Heap
>   , empty
>   , update
>     -- Sequential evaluation monad
>   , SeqEval
>   , seqLoad
>   , seqStore
>   , seqRmw
>   , seqCmpxchg
>     -- Error
>   , EvalError (VarError)
>   )
> where
> import qualified Data.Functor.Foldable as F
> import Control.Applicative (liftA2)
> import Control.Monad.Except (throwError)
> import Control.Monad.State (StateT, gets, modify, runStateT, evalStateT)
> import Data.Bits (complement)
> import Data.Int (Int32)
> import Data.Function (on)
> import Language.C4.Fir.Const (Const (..), bool, i32, coerceBool, coerceI32)
> import Language.C4.Fir.Expr (Expr (..), ExprF (..), PrimExpr (..) )
> import qualified Language.C4.Fir.Op as Op
> import Language.C4.Fir.Lvalue (NormAddress, normalise)
> import Language.C4.Fir.MemOrder (MemOrder (..))

> -- | Monad capturing enough of the memory model to evaluate expressions.
> class Monad m => MonadEval m where
>   -- | Loads a heap value, potentially atomically.
>   load :: Maybe MemOrder -> NormAddress -> m Const
>   -- | Stores a heap value, potentially atomically.
>   store :: Maybe MemOrder -> NormAddress -> Const -> m ()
>   -- | Atomically reads, modifies, and writes a heap value.
>   rmw :: MemOrder -> NormAddress -> (Const -> Const) -> m Const
>   {- | Compare and exchange: `cmpxchg succ fail obj expected desired` has
>        the same semantics as `atomic_compare_exchange_strong_explicit`. -}
>   cmpxchg :: MemOrder -> MemOrder -> NormAddress -> NormAddress -> Const -> m Bool

Sequential evaluation
---------------------

The sequential evaluation monad is a state transformer over evaluation errors,
where the state is a heap modelled as a partial function from addresses to
constants.

> -- | Model of expression evaluation with sequential semantics.
> type SeqEval = StateT Heap (Either EvalError)

> -- | A shallow heap model.
> type Heap = NormAddress -> Maybe Const

> -- | The empty heap.
> empty :: Heap
> empty = const Nothing

We update a heap by producing a new function that checks the new mapping first,
and then delegates to the old heap.  This is quite inefficient, but at time of
writing we don't expect the Seq heap model to be used very extensively.

> -- | Updates a heap with a new mapping.
> update :: NormAddress -> Const -> Heap -> Heap
> update a k h a' = if a == a' then Just k else h a'

The monad implements MonadEval, but does so by ignoring memory orders and
propagating heap changes immediately.

> instance MonadEval SeqEval where
>   load _ = seqLoad
>   store _ = seqStore
>   rmw _ = seqRmw
>   cmpxchg _ _ = seqCmpxchg

We now give the definitions of the various Seq heap operations.  Since we're
just assuming that heap updates propagate immediately without any

> -- | Loads an address in the Seq evaluation model.
> seqLoad :: NormAddress -> SeqEval Const
> seqLoad a = gets ($ a) >>= seqTag (VarError a)

ScTag just 'tags' a partial evaluation with an error.

> {- | Lifts a partial evaluation into the `SeqEval` monad by tagging failure to
>      evaluate with an error. -}
> seqTag :: EvalError -> Maybe a -> SeqEval a
> seqTag e = maybe (throwError e) return

> -- | Stores a constant to an address in the Seq evaluation model.
> seqStore :: NormAddress -> Const -> SeqEval ()
> seqStore a = modify . update a

We can define the various read-modify-write actions through loads and stores:

> -- | Read-modify-writes a constant at an address in the Seq evaluation model.
> seqRmw :: NormAddress -> (Const -> Const) -> SeqEval Const
> seqRmw a f = seqLoad a >>= modWrite
>   where modWrite v = v <$ seqStore a (f v)

> -- | Performs a C-style compare-exchange in the Seq evaluation model.
> seqCmpxchg :: NormAddress -> NormAddress -> Const -> SeqEval Bool
> seqCmpxchg o e d =
>   do
>     ov <- seqLoad o
>     ev <- seqLoad e
>     if ov == ev
>     then True <$ seqStore o d
>     else False <$ seqStore e ov

Evaluating expressions
----------------------

> -- | Evaluates an expression.
> evalExpr :: MonadEval m => Expr a -> m Const

We use a recursion-schemes fold, whereby at each step of evaluation we have
reduced any sub-term into `m Const`: in other words, the raw computation that
will result in a constant value if we evaluate it.  (We can't evaluate to a
`Const` in a recursion because of short-circuiting in some operators.)

> evalExpr = F.fold evalExpr'
>   where

Evaluation ignores metadata.

>     evalExpr' (MetaF _ k) = k

Every other leg delegates to several sub-evaluators we define below.

>     evalExpr' (PrimF p) = evalPrim p
>     evalExpr' (BinF o l r) = evalBop o l r
>     evalExpr' (UnF o x) = evalUop o x

> -- | Evaluates an expression in a sequential heap,
> --   returning the new heap and constant final value on success.
> seqEvalExpr' :: Expr a -> Heap -> Either EvalError (Const, Heap)
> seqEvalExpr' = runStateT . evalExpr

> -- | Evaluates an expression in a sequential heap,
> --   returning the constant final value on success.
> seqEvalExpr :: Expr a -> Heap -> Either EvalError Const
> seqEvalExpr = evalStateT . evalExpr

Evaluating primitives
---------------------

> -- | Evaluates a primitive expression.
> evalPrim :: MonadEval m => PrimExpr -> m Const

Evaluating constants is trivial: we just return them.

> evalPrim (Con x) = pure x

To evaluate addresses, we normalise them, then look them up in the current
heap with non-atomic semantics.

> evalPrim (Addr a) = load Nothing (normalise a)

Evaluating operators
--------------------

Arithmetic and bitwise operators map, purely, from integers to integers,
so they share most of their boilerplate.

> -- | Provides the main shape of a binary operator evaluator.
> evalBopGen
>   :: MonadEval m
>   => (Const -> a)             -- ^ A lifter for argument constants.
>   -> (b -> Const)             -- ^ An unlifter for the final value.
>   -> (m a -> m a -> m b)      -- ^ The semantics of the operator.
>   -> m Const                  -- ^ The left argument.
>   -> m Const                  -- ^ The right argument.
>   -> m Const                  -- ^ The final result.
> evalBopGen arg res f = evalBopGen' `on` (arg <$>)
>   where evalBopGen' l r = res <$> f l r

> -- | Evaluates an arithmetic binary operation.
> evalABop :: MonadEval m => Op.ABop -> m Const -> m Const -> m Const
> evalABop = evalBopGen coerceI32 i32 . liftA2 . Op.semABop

> -- | Evaluates a bitwise binary operation.
> evalBBop :: MonadEval m => Op.BBop -> m Const -> m Const -> m Const
> evalBBop = evalBopGen coerceI32 i32 . liftA2 . Op.semBBop

Logical operations map from Booleans to Booleans, but carry through the monad,
so as to allow for short-circuit evaluation.

> -- | Evaluates a logical binary operation.
> evalLBop :: MonadEval m => Op.LBop -> m Const -> m Const -> m Const
> evalLBop = evalBopGen coerceBool bool . Op.semLBop

Finally, relational operations need a degree of agreement as to what the
type is.  For now, we just coerce both sides to Int32, as it forms a
strict extension of booleans; if we get multiple bit widths, we may need to
rethink this.

> -- | Evaluates a relational binary operation.
> evalRBop :: MonadEval m => Op.RBop -> m Const -> m Const -> m Const
> evalRBop = evalBopGen coerceI32 bool . liftA2 . Op.semRBop

> -- | Evaluates a binary operator.
> evalBop :: MonadEval m => Op.Bop -> m Const -> m Const -> m Const
> evalBop (Op.Arith o) = evalABop o
> evalBop (Op.Bitwise o) = evalBBop o
> evalBop (Op.Logical o) = evalLBop o
> evalBop (Op.Rel o) = evalRBop o

> -- | Evaluates a unary operator.
> evalUop :: MonadEval m => Op.Uop -> m Const -> m Const
> evalUop Op.Comp = fmap (i32 . complement . coerceI32)
> evalUop Op.Not = fmap (bool . not . coerceBool)

Evaluation errors
-----------------

> -- | Enumeration of possible errors when evaluating an expression.
> data EvalError
>   = VarError NormAddress
>     deriving (Eq, Show)
