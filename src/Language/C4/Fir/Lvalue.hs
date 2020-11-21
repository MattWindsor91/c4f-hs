{-# LANGUAGE DeriveFunctor, TemplateHaskell, TypeFamilies #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Lvalue
-- Description : Lvalues and addresses in FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------

module Language.C4.Fir.Lvalue
  ( -- * Key definitions
    Address (Lv, Ref)
  , Lvalue  (Var, Deref)
    -- ** Recursion schemes
  , AddressF (LvF, RefF)
  , LvalueF  (VarF, DerefF)
    -- ** Optics
  , _Lv
  , _Ref
  , _Var
  , _Deref
    -- * Normalised addresses
  , NormAddress
  , normalise
  , denormalise
    -- * Non-typesafe generators
  , genLvalue'
  , genLvalue
  , genAddress'
  , genAddress
  ) where

import qualified Control.Lens as LL
import qualified Data.Functor.Foldable as F
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as G

import Language.C4.Fir.Id (Id, AsId, _Id, HasId, underlyingId, gen)

-- | Lvalues.
data Lvalue
  = Var   Id     -- ^ A variable reference.
  | Deref Lvalue -- ^ A dereference of another lvalue.
    deriving (Eq, Show)
LL.makePrisms ''Lvalue

-- An lvalue can directly contain an identifier, as follows:
instance AsId Lvalue where _Id = _Var

-- | Base functor for recursion on 'Lvalue'.
data LvalueF b
  = VarF   Id -- ^ Non-recursive form of 'Var'.
  | DerefF b  -- ^ Non-recursive form of 'Deref'.
    deriving Functor

-- The usual `recursion-schemes` boilerplate follows:
type instance F.Base Lvalue = LvalueF
instance F.Recursive Lvalue where
  project (Var   v) = VarF v
  project (Deref d) = DerefF d
instance F.Corecursive Lvalue where
  embed (VarF   v) = Var v
  embed (DerefF d) = Deref d

-- | Lens projecting into the underlying identifier of the lvalue.
--
--   This is distinct from '_Var', which matches only lvalues that are
--   non-dereferenced variables, and doesn't project through defererences.
lvalueId :: LL.Lens' Lvalue Id
lvalueId f = F.fold lvalueId'
  where lvalueId' (VarF x) = Var <$> f x
        lvalueId' (DerefF x) = Deref <$> x
instance HasId Lvalue where underlyingId = lvalueId

-- | Addresses.
data Address
  = Lv  Lvalue  -- ^ An lvalue as an address.
  | Ref Address -- ^ A reference to another address.
    deriving (Eq, Show)
LL.makePrisms ''Address

-- An address is an identifier if it is `Lv (Var x)` for some @x@.
instance AsId Address where _Id = _Lv . _Var

-- | Base functor for recursion on 'Address'.
data AddressF b
  = LvF Lvalue
  | RefF b
    deriving Functor

-- The usual `recursion-schemes` boilerplate follows:
type instance F.Base Address = AddressF
instance F.Recursive Address where
  project (Lv  l) = LvF l
  project (Ref r) = RefF r
instance F.Corecursive Address where
  embed (LvF  l) = Lv l
  embed (RefF r) = Ref r

-- | Lens projecting into the underlying identifier of the address.
addressId :: LL.Lens' Address Id
{- We could define 'HasId' by composing a retrieval of the lvalue of an address
   and the variable of the lvalue, but this seems slightly more optimal. -}
addressId f = F.fold addressId'
  where addressId' (LvF x) = Lv <$> lvalueId f x
        addressId' (RefF x) = Ref <$> x
instance HasId Address where underlyingId = addressId

-- | Normalised addresses.
--
-- Addresses are not normalised by default: for instance, if @a@ is a pointer,
-- @a@, @&*a@, and @&&**a@ all point to the same bit of memory.  This is an
-- issue for things like modelling heaps as address maps.
--
-- Create normalised addresses using 'normalise', and lower them back to
-- addresses using 'denormalise'.
newtype NormAddress = Norm Address deriving (Eq, Show)

-- | Normalises an address.
--
-- Addresses normalise recursively using the following rules:
--
-- - A raw lvalue is already normalised.
-- - A reference of a normalised dereference, ie @&*x@, becomes @x@.
-- - Any other reference of a normalised value is normalised.
normalise :: Address -> NormAddress
normalise = F.fold normalise'
  where normalise' (LvF x)                      = Norm (Lv  x)
        normalise' (RefF (Norm (Lv (Deref x)))) = Norm (Lv  x)
        normalise' (RefF (Norm x))              = Norm (Ref x)

-- | Forgets that a normalised address is normalised.
denormalise :: NormAddress -> Address
denormalise (Norm x) = x

{-
 - Non-typesafe generators
 -}

-- | Generates random non-typesafe lvalues given an identifier generator.
genLvalue' :: MonadGen m => m Id -> m Lvalue
genLvalue' f = G.recursive G.choice
  [ Var <$> f ]
  [ G.subterm (genLvalue' f) Deref ]

-- | Generates random non-typesafe lvalues.
genLvalue :: MonadGen m => m Lvalue
genLvalue = genLvalue' gen

-- | Generates random non-typesafe addresses given an lvalue generator.
genAddress' :: MonadGen m => m Lvalue -> m Address
genAddress' f = G.recursive G.choice
  [ Lv <$> f ]
  [ G.subterm (genAddress' f) Ref ]

-- | Generates random non-typesafe addresses.
genAddress :: MonadGen m => m Address
genAddress = genAddress' genLvalue
