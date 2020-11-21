{-# LANGUAGE DeriveFunctor, TemplateHaskell, KindSignatures #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Atomic.MemOrder
-- Description : C4 Fuzzable Internal Representation: Memory orders
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module enumerates the C11 memory orders.
--------------------------------------------------------------------------------

module Language.C4.Fir.Atomic.MemOrder
  ( MemOrder
      ( Relaxed
      , Consume
      , Acquire
      , Release
      , AcqRel
      , SeqCst
      )  
    -- * 'MemOrder' optics
  , AsMemOrder
  , _MemOrder
  , _Relaxed
  , _Consume
  , _Acquire
  , _Release
  , _AcqRel
  , _SeqCst
    -- * Memory order arguments
  , MemOrderArg (Implicit, Explicit)
    -- * 'MemOrderArg' optics
  , _Implicit
  , _Explicit
  , _moMaybe
    -- * Compound memory orders
  , CmpxchgMemOrder (CmpxchgMemOrder, _success, _failure)
    -- * 'CmpxchgMemOrder' optics
  , success
  , failure
  ) where

import Control.Lens ((^?), Iso', iso, makeLenses, makePrisms, makeClassyPrisms)

-- | Enumeration of all memory orders.
--
--   'Ord' on 'MemOrder' is almost a valid total ordering on memory order
--   strength, except that (for instance) it arbitrarily orders 'release' after
--   'acquire'.
data MemOrder
  = Relaxed -- ^ C11 `mem_order_relaxed`.
  | Consume -- ^ C11 `mem_order_consume`.
  | Acquire -- ^ C11 `mem_order_acquire`.
  | Release -- ^ C11 `mem_order_release`.
  | AcqRel -- ^ C11 `mem_order_acq_rel`.
  | SeqCst -- ^ C11 `mem_order_seq_cst`.
    deriving (Eq, Ord, Show)
makeClassyPrisms ''MemOrder

-- | A memory order argument, parametrised on either a memory order or an
--   expression or container contaning them.
--   some other memory order container
data MemOrderArg e
  = Implicit   -- ^ An implicit memory order (ie, sequential consistency).
  | Explicit e -- ^ An explicit memory order.
    deriving (Eq, Ord, Functor, Show)
makePrisms ''MemOrderArg

-- | 'MemOrderArg' is isomorphic to 'Maybe e', where 'e' is the memory order
--   expression.
_moMaybe :: Iso' (MemOrderArg e) (Maybe e)
_moMaybe = iso toMaybe fromMaybe
  where toMaybe x = x ^? _Explicit
        fromMaybe = maybe Implicit Explicit

-- | A memory order pair for a compare-exchange.
data CmpxchgMemOrder e
  = CmpxchgMemOrder
      { _success :: e -- ^ Memory order on success.
      , _failure :: e -- ^ Memory order on failure.
      } deriving (Eq, Ord, Functor, Show)
makeLenses ''CmpxchgMemOrder
