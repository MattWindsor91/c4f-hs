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
>   ( MonadEval
>   , SeqEval
>   )
> where
> import Control.Monad.Except (throwError)
> import Control.Monad.State (StateT, gets, modify)
> import Language.C4.Fir.Const (Const (..))
> import Language.C4.Fir.Expr (Expr (..), PrimExpr (..) )
> import Language.C4.Fir.Op
>   (semABop, semBBop, semLBop, semRBop, Bop (..), Uop (..))
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

Evaluation errors
-----------------

> data EvalError
>   = TypeError
>   | VarError NormAddress
