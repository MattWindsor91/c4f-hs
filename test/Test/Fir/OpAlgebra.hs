{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Test.Fir.OpAlgebra
Description : Tests for Language.C4.Fir.OpAlgebra
Copyright   : (c) Matt Windsor, 2018, 2019, 2020
License     : MIT
Maintainer  : mattwindsor91@gmail.com
Stability   : experimental
-}
module Test.Fir.OpAlgebra
  (tests) where

import qualified Control.Lens as L
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Language.C4.Fir.OpAlgebra as Src
import qualified Language.C4.Fir.Op as Op
import qualified Language.C4.Fir.Expr as Expr
import qualified Language.C4.Fir.Eval as Eval

prop_abopRulesValid :: Property
prop_abopRulesValid = property $
  do (op, term) <- forAll (Gen. Op.ABop)
     i <- forAll Gen.int32

     



tests :: IO Bool
tests =
  checkSequential $$(discover)
