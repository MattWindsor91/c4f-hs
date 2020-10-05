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
>   ( Const (Int, Bool)
>     -- Lenses
>   , AsConst
>   , _Const -- :: AsConst s => Prism' Const s
>   , _Int -- :: AsConst s => Prism' Integer s
>   , _Bool -- :: AsConst s => Prism' Bool s
>     -- Convenience constructors
>   , int -- :: AsConst s => Integer -> s
>   , bool -- :: AsConst s => Bool -> s
>   , true -- :: AsConst s => s
>   , false -- :: AsConst s => s
>   , zero -- :: AsConst s => s
>   )
> where
> import Control.Lens (makeClassyPrisms, review)

Fir supports (signed) integer and Boolean ("true", "false") constants.

> data Const
>   = Int Integer
>   | Bool Bool
>     deriving (Eq, Show)

This TemplateHaskell incantation gives us an `AsConst` type class which we
can use to manipulate larger expression types as if they are `Const`.

> makeClassyPrisms ''Const

Convenience constructors
------------------------

We often want to build a constant, primitive expression, or expression that
has a particular constant integer or Boolean value.  We define a few shorthand
constructors on `AsConst` to make this easier.

> -- | Lifts an integer to a constant (or constant expression).
> int :: AsConst k => Integer -> k
> int = review _Int

> -- | Constructs a constant zero.
> zero :: AsConst k => k
> zero = int 0

> -- | Lifts a Boolean to a constant (or constant expression).
> bool :: AsConst k => Bool -> k
> bool = review _Bool

> -- | Constructs a constant true.
> true :: AsConst k => k
> true = bool True

> -- | Constructs a constant false.
> false :: AsConst k => k
> false = bool False
