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
  ) where

-- | Enumeration of all memory orders.
data MemOrder
  = Relaxed -- ^ C11 `mem_order_relaxed`.
  | Consume -- ^ C11 `mem_order_consume`.
  | Acquire -- ^ C11 `mem_order_acquire`.
  | Release -- ^ C11 `mem_order_release`.
  | AcqRel -- ^ C11 `mem_order_acq_rel`.
  | SeqCst -- ^ C11 `mem_order_seq_cst`.
    deriving (Eq, Ord, Show)
