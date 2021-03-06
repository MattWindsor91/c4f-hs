{-# LANGUAGE DeriveTraversable, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Atomic.Action
-- Description : Atomic actions for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module defines the structures of atomic actions.
--------------------------------------------------------------------------------

module Language.C4.Fir.Atomic.Action
  ( Load
    ( Load, _src, _mo )
    -- * Optics
  , src
  , mo
  ) where

import qualified Control.Lens as L

import qualified Language.C4.Fir.Lvalue as LV
import qualified Language.C4.Fir.Atomic.MemOrder as M

-- | Type of atomic loads.
data Load e
  = Load { _src :: LV.Address
           -- ^ The source address.
         , _mo  :: M.MemOrderArg e
           -- ^ The memory order argument.
         } deriving (Eq, Functor, Foldable, Traversable, Show)
L.makeLenses ''Load
