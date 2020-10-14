{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Test.Fir.Const
Description : Tests for Language.C4.Fir.Const
Copyright   : (c) Matt Windsor, 2018, 2019, 2020
License     : MIT
Maintainer  : mattwindsor91@gmail.com
Stability   : experimental
-}
module Test.Fir.Const
  (tests)
where
import qualified Control.Lens as L
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Language.C4.Fir.Const as Src

{- | Converting a Boolean to an integer and back is the identity.
     (The opposite is not necessarily true, eg -1 will come back as 1.) -}
prop_coerceBoolIntTrip :: Property
prop_coerceBoolIntTrip = property $
  forAll Gen.bool >>= \x -> tripping x boolInt intBool
  where
    boolInt = Src.coerceInt . L.review Src._Bool
    intBool = L.Identity . Src.coerceBool . L.review Src._Int

tests :: IO Bool
tests =
  checkSequential $$(discover)
