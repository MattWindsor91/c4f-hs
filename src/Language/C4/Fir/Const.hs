{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Const
-- Description : C4 Fuzzable Internal Representation: Constants
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- Fir constants, convenience constructors, and type coersion.
--
-- The general rule for Boolean coersion is that 0 is false and everything else
-- is true; when converting integers to booleans, we further specificially map
-- true to 1.
--------------------------------------------------------------------------------

module Language.C4.Fir.Const
  ( Const (I32, Bool)
    -- * Lenses
  , AsConst
  , _Const
  , _I32
  , _Bool
    -- * Convenience constructors
  , i32
  , bool
  , true
  , false
  , zero
    -- * Coercion
  , coerceI32
  , coerceBool
    -- * Generators
  , gen
  ) where

import Data.Int (Int32)
import Control.Lens (makeClassyPrisms, review)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Fir constants.
--
-- Fir supports (signed) integer and Boolean ("true", "false") constants.
-- These are ordered, though the order only makes intuitive sense in terms of
-- comparing constants of the same type; it mainly exists to let us use
-- constants as keys.
data Const
  = Bool Bool -- ^ Booleans.
  | I32 Int32 -- ^ 32-bit integers.
    deriving (Eq, Ord, Show)

-- This TemplateHaskell incantation gives us an `AsConst` type class which we
-- can use to manipulate larger expression types as if they are `Const`.
makeClassyPrisms ''Const

{-
 - Convenience constructors
 -}

-- | Lifts a 32-bit integer to a constant (or constant expression).
i32 :: AsConst k => Int32 -> k
i32 = review _I32

-- | Constructs a constant zero.
zero :: AsConst k => k
zero = i32 0

-- | Lifts a Boolean to a constant (or constant expression).
bool :: AsConst k => Bool -> k
bool = review _Bool

-- | Constructs a constant true.
true :: AsConst k => k
true = bool True

-- | Constructs a constant false.
false :: AsConst k => k
false = bool False

{-
 - C11-semantics coersion
 -}

-- | Coerces a constant to a Boolean value, using C11-style semantics.
coerceBool :: Const -> Bool
coerceBool (Bool x) = x
coerceBool (I32  x) = intToBool x

-- | Coerces a constant to an integer value, using C11-style semantics.
coerceI32 :: Const -> Int32
coerceI32 (I32  x) = x
coerceI32 (Bool x) = boolToInt x

-- | Interpret integers of any width as Booleans using C11-style semantics.
intToBool :: (Num a, Eq a) => a -> Bool
intToBool = (/= 0)

-- | Interpret Booleans as integers of any width, using C11-style semantics.
boolToInt :: Num a => Bool -> a
boolToInt False = 0
boolToInt True  = 1

{-
 - Generators
 -}

-- | Generates random constants or constant expressions using the given integer
--   ranges.
--
--   This generator shrinks towards false for Booleans, and the origin for 
--   integers (typically 0).
gen :: (AsConst k, MonadGen m)
    => Range.Range Int32 -- ^ Range to use for I32.
    -> m k               -- ^ The generator monad.
gen r32 = Gen.choice [ i32 <$> Gen.int32 r32
                     , bool <$> Gen.bool
                     ]
