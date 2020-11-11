{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Test.Fir.OpAlgebra
-- Description : Tests for Language.C4.Fir.OpAlgebra
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------
module Test.Fir.OpAlgebra
  (tests) where

import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Control.Lens ((#))
import qualified Control.Lens as L
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.C4.Fir.Const as K
import qualified Language.C4.Fir.OpAlgebra as Src
import qualified Language.C4.Fir.Expr.Op as Op
import qualified Language.C4.Fir.Expr.Eval as E
import qualified Language.C4.Fir.Expr.Expr as X
import qualified Language.C4.Fir.Heap as H

-- | Evaluates an expression under the empty heap.
evalEmpty :: X.Expr () -> Either E.EvalError K.Const
evalEmpty = (`H.seqEvalExpr` H.empty)

-- | Property checking if two expressions evaluate to the same value on the
--   empty heap.
evalSameAs :: MonadTest m => X.Expr () -> X.Expr () -> m ()
evalSameAs = (===) `on` evalEmpty

-- | Skeleton for properties that test rule validity.
rulesValidProp
  :: Show o
  => Gen (X.Expr ())                  -- ^ Generator for input constants.
  -> (o -> Op.Bop)                    -- ^ Lifter for operators.
  -> Src.RuleSet o (Src.BIn Src.Term) -- ^ The rule set to investigate.
  -> PropertyT IO ()                  -- ^ The test proper.
rulesValidProp ins f rset =
  do (op, inp, outp) <- forAll rules
     i               <- forAll ins
     (X.Bin (f op) `Src.onBIn` Src.fill (pure i) inp) `evalSameAs` Src.subst i outp
  where rules  = Gen.element (Src.allRules rset)

-- | Checks validity of arithmetic bop rules.
prop_abopRulesValid :: Property
prop_abopRulesValid = property $
  rulesValidProp (K.i32 <$> Gen.int32 Range.constantBounded) Op.Arith Src.arithRules

-- | Checks validity of bitwise bop rules.
prop_bbopRulesValid :: Property
prop_bbopRulesValid = property $
  rulesValidProp (K.i32 <$> Gen.int32 Range.constantBounded) Op.Bitwise Src.bitwiseRules

-- | Checks validity of logical bop rules.
prop_lbopRulesValid :: Property
prop_lbopRulesValid = property $
  rulesValidProp (K.bool <$> Gen.bool) Op.Logical Src.logicalRules

-- | Checks validity of relational bop rules.
prop_rbopRulesValid :: Property
prop_rbopRulesValid = property $
  rulesValidProp (K.gen Range.constantBounded) Op.Rel Src.relRules

-- | Runs tests for OpAlgebra.
tests :: IO Bool
tests =
  checkSequential $$(discover)
