{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Stmt.Stmt
-- Description : Tests for Language.C4.Fir.Stmt.Stmt
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------
module Test.Fir.Stmt.Stmt (tests) where

import qualified Control.Lens as L
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.C4.Fir.Stmt.Stmt as Src

-- | Bimap law for 'StmtBlock'.
prop_bimapStmtBlock :: Property
prop_bimapStmtBlock = property $
  -- We don't need to generate actually sensible metadata and statements!
  do _blockMeta  <- forAll (Gen.int Range.linearBounded)
     _blockStmts <- forAll (Gen.list (Range.linear 0 1000) $
                            Gen.int Range.linearBounded)
     let block = Src.StmtBlock { Src._blockMeta, Src._blockStmts }
     L.bimap id id block === block

tests :: IO Bool
tests = checkSequential $$(discover)
