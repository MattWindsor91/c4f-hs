{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Test.Fir.Lvalue
Description : Tests for Language.C4.Fir.Lvalue
Copyright   : (c) Matt Windsor, 2018, 2019, 2020
License     : MIT
Maintainer  : mattwindsor91@gmail.com
Stability   : experimental
-}
module Test.Fir.Lvalue
  (tests)
where
import qualified Language.C4.Fir.Lvalue as Src
import Hedgehog

tests :: IO Bool
tests =
  checkSequential $$(discover)
