{-# LANGUAGE DeriveTraversable, TemplateHaskell, KindSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Atomic.MemOrder
-- Description : Memory order encoding for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module enumerates the C11 memory orders, as well as various containers
-- of memory orders used elsewhere in FIR.
--
-- Generally, a memory order argument to a FIR node ('MemOrderArg') doesn't
-- directly reference a memory order ('MemOrder'), but instead contains an
-- /expression/ where 'MemOrder' occurs as a constant.  The reason for this is
-- because, surprisingly, C compilers have specific support for cases where
-- memory order arguments are non-constant-foldable expressions.
--
-- For more information about memory orders, see
-- <https://en.cppreference.com/w/c/atomic/memory_order>.
--------------------------------------------------------------------------------

module Language.C4.Fir.Atomic.MemOrder
  ( -- * Memory orders
    MemOrder
      ( Relaxed
      , Consume
      , Acquire
      , Release
      , AcqRel
      , SeqCst
      )  
    -- ** Optics
  , AsMemOrder
  , _MemOrder
  , _Relaxed
  , _Consume
  , _Acquire
  , _Release
  , _AcqRel
  , _SeqCst
    -- * OpenCL memory scopes
  , MemScope
      ( WorkItem
      , WorkGroup
      , Device
      , AllSvmDevices
      )
    -- * Memory order arguments
  , MemOrderArg (Implicit, Explicit)
    -- ** Optics
  , _Implicit
  , _Explicit
  , _moMaybe
    -- * Compound memory orders
  , CmpxchgMemOrder (CmpxchgMemOrder, _success, _failure)
    -- ** Optics
  , success
  , failure
    -- * Generators
  , genMemOrder
  , genMemOrderArg
  , genMemScope
  ) where

import Control.Lens (Iso', iso, makeLenses, makePrisms, makeClassyPrisms)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen

-- | Enumeration of all memory orders.
--
data MemOrder
  = Relaxed -- ^ C11 @memory_order_relaxed@.
  | Consume -- ^ C11 @memory_order_consume@.
  | Acquire -- ^ C11 @memory_order_acquire@.
  | Release -- ^ C11 @memory_order_release@.
  | AcqRel  -- ^ C11 @memory_order_acq_rel@.
  | SeqCst  -- ^ C11 @memory_order_seq_cst@.
    deriving ( Eq
             , Ord
             -- ^ This is almost a valid total ordering on memory order
             --   strength, except that (for instance) it arbitrarily orders
             --   'Release' after 'Acquire'.
             , Enum
             , Bounded
             , Show
             )
makeClassyPrisms ''MemOrder

-- Source:
-- https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/memory_scope.html

-- | OpenCL memory scopes.
--
--   C11 semantics is equivalent to 'Device'.
--
data MemScope
  = WorkItem      -- ^ Scoped to the work-item.
  | WorkGroup     -- ^ Scoped to the work-group.
  | Device        -- ^ Scoped to the device; usually the default scope.
  | AllSvmDevices -- ^ Scoped across devices.
    deriving ( Eq
             , Ord
             -- ^ There is no semantic meaning to this; it exists to allow for
             --   use in maps etc.
             , Enum
             , Bounded
             , Show
             )

-- | A memory order argument, parametrised on either a memory order or an
--   expression or container contaning them.
data MemOrderArg e
  = Implicit            -- ^ An implicit memory order (sequential consistency).
  | Explicit e          -- ^ An explicit memory order.
  | Scoped   e MemScope -- ^ An explicit memory order with OpenCL scope.
    deriving ( Eq
             , Ord
             -- ^ There is no semantic meaning to this; it exists to allow for
             --   use in maps etc.
             , Functor
             , Foldable
             , Traversable
             , Show
             )
makePrisms ''MemOrderArg

-- | 'MemOrderArg' is isomorphic to a nested pair of 'Maybe's capturing the
--   three different possibilities
_moMaybe :: Iso' (MemOrderArg e) (Maybe (e, Maybe MemScope))
_moMaybe = iso toMaybe fromMaybe
  where toMaybe Implicit       = Nothing
        toMaybe (Explicit e  ) = Just (e, Nothing)
        toMaybe (Scoped   e s) = Just (e, Just s)
        fromMaybe Nothing             = Implicit
        fromMaybe (Just (e, Nothing)) = Explicit e
        fromMaybe (Just (e, Just s )) = Scoped   e s

-- | A memory order pair for a compare-exchange.
data CmpxchgMemOrder e
  = CmpxchgMemOrder
      { _success :: e -- ^ Memory order on success.
      , _failure :: e -- ^ Memory order on failure.
      } deriving (Eq, Ord, Functor, Foldable, Traversable, Show)
makeLenses ''CmpxchgMemOrder

{-
 - Generators
 -}

-- | Generator for arbitrary memory orders.
genMemOrder :: MonadGen m => m MemOrder
genMemOrder = Gen.enumBounded

-- TODO(@MattWindsor91): operation-safe generators

-- | Generator for arbitrary memory scopes.
genMemScope :: MonadGen m => m MemScope
genMemScope = Gen.enumBounded

-- | Parametric generator for memory order arguments.
genMemOrderArg
  :: MonadGen m
  => m e               -- ^ Generator for memory order expressions.
  -> m MemScope        -- ^ Generator for memory scopes.
  -> m (MemOrderArg e) -- ^ Generator for memory order arguments.
genMemOrderArg genO genS =
  -- Shrinking towards the most implicit definitions.
  Gen.choice
    [ pure Implicit
    , Explicit <$> genO
    , Scoped   <$> genO <*> genS
    ]
