--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Type
-- Description : FIR type system
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- The FIR type system is basically that of C11, but with more emphasis on
-- sized integers.
--------------------------------------------------------------------------------
module Language.C4.Fir.Type
  ( -- * Properties of types
    IntSize (Size32)
  , Signedness (Signed)
  , Indirection (Direct, PointerTo)
  , Atomicity (Atomic, NonAtomic)
    -- * Types of types
  , SType (TBool, TInt)
  , PType (TScalar)
  , Type  (Type, _pType, _atomicity, _indirection)
  , AType
  , NType
    -- * Reflection from Haskell types to FIR types
  , liftHsType
  ) where

import Data.Int (Int32)
import Data.Functor (($>))
import Data.Maybe (catMaybes, listToMaybe)
import qualified Type.Reflection as TR

{-
 - Properties of types
 -}

-- | The sizes of integer known to C4.
data IntSize = Size32 -- ^ 32-bit.
    deriving (Eq, Show)

-- | Enumeration of signedness of integer types.
--
--   Currently, this only contains signed integers; unsigned integers may
--   follow.
data Signedness = Signed -- ^ Signed integers.
    deriving (Eq, Show)

-- | Levels of indirection of types.
--
--   Currently, we only allow one level of indirection.
data Indirection
  = Direct    -- ^ Not a pointer or reference.
  | PointerTo -- ^ A pointer.
    deriving (Eq, Show)

-- | Levels of atomicity of types.
data Atomicity
  = Atomic    -- ^ Atomic type.
  | NonAtomic -- ^ Non-atomic type1.
    deriving (Eq, Show)

{-
 - Types of types
 -}

-- | Scalar types.
data SType
  = TBool        -- ^ Boolean type.
  | TInt IntSize -- ^ Integer type with given size.
    deriving (Eq, Show)

-- | Primitive types.
--
-- This type mainly exists to give room for future struct/union/array expansion.
newtype PType = TScalar SType -- ^ Scalar type.
    deriving (Eq, Show)

-- | Full types, parametrised on notions of atomicity and indirection.
data Type a i = Type
  { _pType       :: PType -- ^ The primitive type.
  , _atomicity   :: a     -- ^ The atomicity of the type.
  , _indirection :: i     -- ^ The indirection of the type.
  } deriving (Eq, Show)

-- | Type synonym for full, potentially-atomic types.
type AType = Type Atomicity Indirection

-- | Type synonym for types that can be indirect, but never atomic.
type NType = Type () Indirection

-- | Tries to lift a Haskell type into a FIR scalar type.
liftHsType :: TR.TypeRep a -> Maybe SType
liftHsType r =
  listToMaybe $ catMaybes
    [ TR.eqTypeRep r (TR.typeRep :: TR.TypeRep Bool) $> TBool
    , TR.eqTypeRep r (TR.typeRep :: TR.TypeRep Int32) $> TInt Size32
    ]
