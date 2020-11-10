--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Heap
-- Description : FIR type system
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- The FIR type system is basically that of C, but with more emphasis on
-- sized integers.
--------------------------------------------------------------------------------
module Language.C4.Fir.Type
  ( -- * Properties of types
    IntSize (Size32)
  , Signedness (Signed)
  , Indirection (Direct, PointerTo)
  , Atomicity (Atomic, NonAtomic)
  , SType (TBool, TInt)
  , PType (TScalar)
  , Type  (Type, _pType, _atomicity, _indirection)
  , AType
  , NType
  ) where

{-
 - Properties of types
 -}

-- | The sizes of integer known to C4.
data IntSize = Size32 -- ^ 32-bit.

-- | Enumeration of signedness of integer types.
--
--   Currently, this only contains signed integers; unsigned integers may
--   follow.
data Signedness = Signed -- ^ Signed integers.

-- | Levels of indirection of types.
--
--   Currently, we only allow one level of indirection.
data Indirection
  = Direct    -- ^ Not a pointer or reference.
  | PointerTo -- ^ A pointer.

-- | Levels of atomicity of types.
data Atomicity
  = Atomic    -- ^ Atomic type.
  | NonAtomic -- ^ Non-atomic type1.

-- | Scalar types.
data SType
  = TBool        -- ^ Boolean type.
  | TInt IntSize -- ^ Integer type with given size.

-- | Primitive types.
--
-- This type mainly exists to give room for future struct/union/array expansion.
newtype PType = TScalar SType -- ^ Scalar type.

-- | Full types, parametrised on notions of atomicity and indirection.
data Type a i = Type
  { _pType       :: PType -- ^ The primitive type.
  , _atomicity   :: a     -- ^ The atomicity of the type.
  , _indirection :: i     -- ^ The indirection of the type.
  }


-- | Type synonym for full, potentially-atomic types.
type AType = Type Atomicity Indirection

-- | Type synonym for types that can be indirect, but never atomic.
type NType = Type () Indirection
