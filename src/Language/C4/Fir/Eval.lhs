Evaluating Fir expressions
==========================

The successful testing of various parts of the fuzzer system depends on the
ability to evaluate expressions (at least, in an idealised sequential version
of the Fir semantics).

> {-|
> Module      : Language.C4.Fir.Eval
> Description : C4 Fuzzable Internal Representation: Expression evaluation
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.Eval
>   ( Eval
>   )
> where
> import Control.Monad.State (StateT{-, asks-})
> import Language.C4.Fir.Const (Const (..))
> import Language.C4.Fir.Expr (Expr (..), PrimExpr (..) )
> import Language.C4.Fir.Lvalue (NormAddress)

The heap model used in evaluation is a partial function from addresses to
constants.

> type Heap = NormAddress -> Maybe Const

The evaluation monad
--------------------

The evaluation monad is a

> type Eval = StateT Heap (Either EvalError)

 access :: Address -> Eval Const
 access a = asks ($ a)

Evaluating primitives
--------------------

 evalPrim :: PrimExpr -> Eval Const

Evaluating primitives is trivial; they just

 evalPrim (Con x) = return x
 evalP

Evaluation errors
-----------------

> data EvalError
>   = TypeError
>   | VarError NormAddress
