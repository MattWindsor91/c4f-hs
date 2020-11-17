{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Atomic.Load
-- Description : C4 Fuzzable Internal Representation: Atomic loads
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module gives a representation of atomic loads based on C11/OpenCL.
--------------------------------------------------------------------------------

module Language.C4.Fir.Atomic.Load
  ( ALoad (ALoad, _src, _mo, _scope)
    -- * Optics
  , src
  , mo
  , scope
  ) where

import Control.Lens (makeLenses)
import Language.C4.Fir.Atomic.MemOrder (MemOrderArg)
import Language.C4.Fir.Atomic.MemScope (MemScope)

-- | An atomic load, predicated over expressions.
data ALoad e =
  ALoad { _src   :: e
          -- ^ The source of the load.
        , _mo    :: MemOrderArg e
          -- ^ The memory order expression for the load.
        , _scope :: MemScope
          -- ^ The OpenCL scope of the load.
        } deriving (Eq, Ord, Show)
makeLenses ''ALoad
