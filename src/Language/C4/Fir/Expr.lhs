Expressions
===========

> {-# LANGUAGE TemplateHaskell, TypeFamilies #-}

> {-|
> Module      : Language.C4.Fir.Expr
> Description : C4 Fuzzable Internal Representation: Expressions
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.Expr
>   ( Expr (Meta, Prim, Bin)
>   , Lvalue (Var, Deref)
>   , PrimExpr (Con, Lv)
>     -- Binary operator shorthand
>   , (@+) -- :: Expr m -> Expr m -> Expr m
>   , (@-) -- :: Expr m -> Expr m -> Expr m
>   , (@<) -- :: Expr m -> Expr m -> Expr m
>   , (@<=) -- :: Expr m -> Expr m -> Expr m
>   , (@>) -- :: Expr m -> Expr m -> Expr m
>   , (@>=) -- :: Expr m -> Expr m -> Expr m
>   , (@==) -- :: Expr m -> Expr m -> Expr m
>   , (@!=) -- :: Expr m -> Expr m -> Expr m
>   , (@&) -- :: Expr m -> Expr m -> Expr m
>   , (@^) -- :: Expr m -> Expr m -> Expr m
>   , (@|) -- :: Expr m -> Expr m -> Expr m
>   , (@&&) -- :: Expr m -> Expr m -> Expr m
>   , (@||) -- :: Expr m -> Expr m -> Expr m
>   )
> where
> import Control.Lens (makePrisms, makeClassyPrisms)
> import Data.Functor.Foldable as F
> import Language.C4.Fir.Const (AsConst, Const, _Const)
> import Language.C4.Fir.Id (Id)
> import Language.C4.Fir.Op
>   (ABop (..), BBop (..), LBop (..), RBop (..), Bop (..))

Lvalues
-------

To move.

> -- | Lvalues.
> data Lvalue
>   = Var Id -- ^ A variable reference.
>   | Deref Lvalue -- ^ A dereference of another lvalue.
>     deriving (Eq, Show)

Primitive expressions
---------------------

> -- | Primitive expressions.
> data PrimExpr
>   = Con Const -- ^ A constant expression.
>   | Lv Lvalue -- ^ An lvalue expression.
>     deriving (Eq, Show)

There doesn't seem to be a good reason for PrimExprs to be classy.

> makePrisms ''PrimExpr

We can turn constant lenses on primitive expressions that hold constants.

> instance AsConst PrimExpr where
>   _Const = _Con

Top-level expressions
---------------------

> -- | Top-level expressions.
> data Expr m
>   = Meta m (Expr m)
>   | Prim PrimExpr
>   | Bin Bop (Expr m) (Expr m)
>     deriving (Eq, Show)

We don't make classy prisms for Exprs, because the introduction of the metadata
parameter causes the resulting class to be very unwieldy.  This may change in
future if we really need it.

> makePrisms ''Expr

> instance AsConst (Expr m) where
>   _Const = _Prim . _Con

Infix operators
---------------

> -- | Constructs an arithmetic binary expression.
> arith :: ABop -> Expr m -> Expr m -> Expr m
> arith = Bin . Arith

> -- | Constructs an addition.
> (@+) :: Expr m -> Expr m -> Expr m
> (@+) = arith (:+)
> infixl 7 @+

> -- | Constructs a subtraction.
> (@-) :: Expr m -> Expr m -> Expr m
> (@-) = arith (:-)
> infixl 7 @-

If we add multiplication, it goes here.

> -- | Constructs a relational binary expression.
> rel :: RBop -> Expr m -> Expr m -> Expr m
> rel = Bin . Rel

> -- | Constructs a less-than.
> (@<) :: Expr m -> Expr m -> Expr m
> (@<) = rel (:<)
> infixl 6 @<

> -- | Constructs a less-than-or-equal.
> (@<=) :: Expr m -> Expr m -> Expr m
> (@<=) = rel (:<=)
> infixl 6 @<=

> -- | Constructs a greater-than.
> (@>) :: Expr m -> Expr m -> Expr m
> (@>) = rel (:>)
> infixl 6 @>

> -- | Constructs a greater-than-or-equal.
> (@>=) :: Expr m -> Expr m -> Expr m
> (@>=) = rel (:>=)
> infixl 6 @>=

> -- | Constructs an equality.
> (@==) :: Expr m -> Expr m -> Expr m
> (@==) = rel (:==)
> infixl 5 @==

> -- | Constructs a non-equality.
> (@!=) :: Expr m -> Expr m -> Expr m
> (@!=) = rel (:!=)
> infixl 5 @!=

> -- | Constructs a bitwise binary expression.
> bitwise :: BBop -> Expr m -> Expr m -> Expr m
> bitwise = Bin . Bitwise

> -- | Constructs a bitwise AND.
> (@&) :: Expr m -> Expr m -> Expr m
> (@&) = bitwise (:&)
> infixl 4 @&

> -- | Constructs a bitwise XOR.
> (@^) :: Expr m -> Expr m -> Expr m
> (@^) = bitwise (:^)
> infixl 3 @^

> -- | Constructs a bitwise OR.
> (@|) :: Expr m -> Expr m -> Expr m
> (@|) = bitwise (:|)
> infixl 2 @|

> -- | Constructs a logical binary expression.
> logical :: LBop -> Expr m -> Expr m -> Expr m
> logical = Bin . Logical

> -- | Constructs a logical AND.
> (@&&) :: Expr m -> Expr m -> Expr m
> (@&&) = Bin (Logical (:&&))
> infixl 1 @&&

> -- | Constructs a logical OR.
> (@||) :: Expr m -> Expr m -> Expr m
> (@||) = Bin (Logical (:||))
> infixl 0 @||

Recursion schemes
-----------------

> data ExprF m b
>   = MetaF m b
>   | PrimF PrimExpr
>   | BinF Bop b b

> type instance F.Base (Expr m) = ExprF m

