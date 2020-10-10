{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

{-|
Module      : Test.Fir.Id
Description : Tests for Language.C4.Fir.Lvalue
Copyright   : (c) Matt Windsor, 2018, 2019, 2020
License     : MIT
Maintainer  : mattwindsor91@gmail.com
Stability   : experimental
-}
module Test.Fir.Id
  (tests)
where

import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as B
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


import qualified Control.Lens as L
import qualified Language.C4.Fir.Id as Src


onId :: (Src.Id -> PropertyT IO ()) -> Property
onId f = property $ forAll Src.gen >>= f

-- | All generated identifiers are valid.
prop_genIdValid :: Property
prop_genIdValid = onId $ assert . Src.isValid . Src.toBytes

-- | `toBytes` and `fromBytes` round-trip.
prop_bytesRoundTrip = onId (\x -> tripping x Src.toBytes Src.fromBytes)

-- | Valid identifiers always convert from bytes.
prop_validFromBytes :: Property
prop_validFromBytes = property $
  forAll genValidAscii >>= assert . isJust . Src.fromBytes

{-
 - Prism laws for `bytes`
 -}

-- | `bytes` obeys the first prism law.
prop_bytesPrismLaw1 :: Property
prop_bytesPrismLaw1 =
  onId (\x -> L.preview Src.bytes (L.review Src.bytes x) === Just x)

-- TODO(@MattWindsor91): second and third prism laws

prop_bytesPrismLaw2 :: Property
prop_bytesPrismLaw2 = property $ do
  s <- forAll genValidAscii
  case L.preview Src.bytes s of
    Just t -> L.review Src.bytes t === s
    Nothing -> discard

prop_bytesPrismLaw3 :: Property
prop_bytesPrismLaw3 = property $ do
  s <- forAll $ Gen.filter (not . Src.isValid) genAscii
  case L.matching Src.bytes s of
    Left t -> L.matching Src.bytes t === Left s
    Right _ -> discard

-- | Generates a random ASCII bytestring.
genAscii :: MonadGen m => m B.ByteString
genAscii = Gen.utf8 (Range.linear 0 100) Gen.ascii

-- | Naively generates an identifier-valid random ASCII bytestring.
genValidAscii :: (MonadGen m, GenBase m ~ Identity) => m B.ByteString
genValidAscii = Gen.filter Src.isValid genAscii

tests :: IO Bool
tests =
  checkSequential $$(discover)
