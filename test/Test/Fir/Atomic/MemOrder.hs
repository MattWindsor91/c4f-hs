{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Atomic.MemOrder
-- Description : Tests for Language.C4.Fir.Atomic.MemOrder
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------
module Test.Fir.Atomic.MemOrder (tests) where

import qualified Control.Lens as L
import Hedgehog
import qualified Language.C4.Fir.Atomic.MemOrder as Src

-- | Round-tripping the '_moMaybe' isomorphism in one direction is the identity.
prop_moMaybeTrip1 :: Property
prop_moMaybeTrip1 = property $
  -- Not generating arbitrary mem-order expressions, because we don't need to.
  forAll (Src.genMemOrderArg Src.genMemOrder Src.genMemScope)
  >>= \m -> tripping m toTuple fromTuple
  where toTuple   = L.view Src._moMaybe
        fromTuple = L.Identity . L.review Src._moMaybe

tests :: IO Bool
tests = checkSequential $$(discover)
