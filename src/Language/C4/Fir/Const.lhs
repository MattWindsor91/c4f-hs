Constants
=========

> {-# LANGUAGE TemplateHaskell #-}

> {-|
> Module      : Language.C4.Fir.Const
> Description : C4 Fuzzable Internal Representation: Constants
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.Const
>   ( Const (I32, Bool)
>     -- Lenses
>   , AsConst
>   , _Const -- :: AsConst k => Prism' Const k
>   , _I32 -- :: AsConst k => Prism' I32 k
>   , _Bool -- :: AsConst k => Prism' Bool k
>     -- Convenience constructors
>   , i32 -- :: AsConst k => Int32 -> k
>   , bool -- :: AsConst k => Bool -> k
>   , true -- :: AsConst k => k
>   , false -- :: AsConst k => k
>   , zero -- :: AsConst k => k
>     -- Coercion
>   , coerceI32 -- :: Const -> Int32
>   , coerceBool -- :: Const -> Bool
>   )
> where
> import Data.Int (Int32)
> import Control.Lens (makeClassyPrisms, review)

Fir supports (signed) integer and Boolean ("true", "false") constants.
These are ordered, though the order only makes intuitive sense in terms of
comparing constants of the same type; it mainly exists to let us use constants
as keys.

> data Const
>   = Bool Bool -- ^ Booleans.
>   | I32 Int32 -- ^ 32-bit integers.
>     deriving (Eq, Ord, Show)

This TemplateHaskell incantation gives us an `AsConst` type class which we
can use to manipulate larger expression types as if they are `Const`.

> makeClassyPrisms ''Const

Convenience constructors
------------------------

We often want to build a constant, primitive expression, or expression that
has a particular constant integer or Boolean value.  We define a few shorthand
constructors on `AsConst` to make this easier.

> -- | Lifts a 32-bit integer to a constant (or constant expression).
> i32 :: AsConst k => Int32 -> k
> i32 = review _I32

> -- | Constructs a constant zero.
> zero :: AsConst k => k
> zero = i32 0

> -- | Lifts a Boolean to a constant (or constant expression).
> bool :: AsConst k => Bool -> k
> bool = review _Bool

> -- | Constructs a constant true.
> true :: AsConst k => k
> true = bool True

> -- | Constructs a constant false.
> false :: AsConst k => k
> false = bool False

Coercion
--------

The `coerceX` functions extract a particular type of constant from a constant
value, using C11-type semantics.

> -- | coerceBool coerces a constant to a Boolean value.
> coerceBool :: Const -> Bool
> coerceBool (Bool x) = x
> coerceBool (I32 x) = intToBool x

> -- | coerceBool coerces a constant to an integer value.
> coerceI32 :: Const -> Int32
> coerceI32 (I32 x) = x
> coerceI32 (Bool x) = boolToInt x

The general rule is that 0 is false and everything else is true; when converting
integers to booleans, we further specificially map true to 1.

> -- | intToBool interprets integers of any width as Booleans,
> --   using C11-style semantics.
> intToBool :: (Num a, Eq a) => a -> Bool
> intToBool = (/= 0)

> -- | boolToInt interprets Booleans as integers of any width,
> --   using C11-style semantics.
> boolToInt :: Num a => Bool -> a
> boolToInt False = 0
> boolToInt True = 1
