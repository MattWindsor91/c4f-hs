--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Atomic.MemScope
-- Description : C4 Fuzzable Internal Representation: Memory orders
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module enumerates the OpenCL memory scopes.
--------------------------------------------------------------------------------

module Language.C4.Fir.Atomic.MemScope
  ( MemScope
      ( WorkItem
      , WorkGroup
      , Device
      , AllSvmDevices
      )
  ) where

-- Source:
-- https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/memory_scope.html

-- | OpenCL memory scopes.
--
--   C11 semantics is equivalent to 'Device'.
--
--   There is no semantic meaning to the 'Ord' instance; it exists to allow for
--   use in maps etc.
data MemScope
  = WorkItem      -- ^ Scoped to the work-item.
  | WorkGroup     -- ^ Scoped to the work-group.
  | Device        -- ^ Scoped to the device; usually the default scope.
  | AllSvmDevices -- ^ Scoped across devices.
    deriving (Eq, Ord, Show)
