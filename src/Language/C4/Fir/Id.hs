--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Id
-- Description : C-style identifier definitions for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------

module Language.C4.Fir.Id
  ( Id
    -- * Conversion to and from bytestrings
  , fromBytes
  , toBytes
    -- * Optics
  , AsId
  , _Id
  , bytes
  , HasId
  , underlyingId
    -- * Generators
  , gen
    -- * Miscellaneous
  , isValid
  )
where
import Data.Char (isAlpha, isAlphaNum)
import qualified Control.Lens as LL
import qualified Control.Lens.Prism as LP
import qualified Data.ByteString.Char8 as C
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

{-
 - Invariants
 -}

-- These two sets of functions should be kept in sync manually.

-- | Captures the C lexing rules on initial characters of C identifiers.
isValidInitial :: Char -> Bool
isValidInitial c = isAlpha c || c == '_'

-- | Generates a valid initial C identifier character.
genInitial :: MonadGen m => m Char
genInitial = G.choice [ G.alpha, return '_' ]

-- | Captures the C lexing rules on non-initial characters of C identifiers.
isValidNonInitial :: Char -> Bool
isValidNonInitial c = isAlphaNum c || c == '_'

-- | Generates a valid non-initial C identifier character.
genNonInitial :: MonadGen m => m Char
genNonInitial = G.choice [ G.alphaNum, return '_' ]

-- | Checks whether a bytestring is a valid identifier.
isValid :: C.ByteString -> Bool
isValid = any isValid' . C.uncons
  where isValid' (c, cs) = isValidInitial c && C.all isValidNonInitial cs

-- | Type of FIR identifiers.
--
-- To enforce C's identifier invariants, we don't expose the internal type.
newtype Id = Id C.ByteString deriving (Eq, Show)

-- | Lowers an identifier to a bytestring.
toBytes :: Id -> C.ByteString
toBytes (Id x) = x

-- | Tries to lift a bytestring to an identifier.
--
-- Returns 'Nothing' if the bytestring does not obey the invariant on
-- identifiers (first character is ASCII alphabetical or @_@; second character
-- is ASCII alphanumeric or @_@).
fromBytes :: C.ByteString -> Maybe Id
fromBytes x = if isValid x then Just (Id x) else Nothing

{-
 - Optics
 -}

-- Trying to derive these using TH doesn't seem to work well.

-- | Typeclass of types that can directly represent identifiers.
class AsId t where
  _Id :: LP.Prism' t Id -- ^ Tries to view the type as an Id.
instance AsId Id where _Id = id

-- | Typeclass of types that can indirectly hold identifiers.
class HasId t where
  underlyingId :: LL.Lens' t Id -- ^ Focuses on the underlying identifier.
instance HasId Id where underlyingId = id

-- | View valid C identifier bytestrings as `Id`s, and vice versa.
bytes :: LP.Prism' C.ByteString Id
bytes = LP.prism' toBytes fromBytes
instance AsId C.ByteString where _Id = bytes

{-
 - Generating IDs
 -}

-- | Generates random, non-human-readable valid identifiers.
gen :: MonadGen m => m Id
gen = make <$> genInitial <*> genNonInitials
  where genNonInitials = C.pack <$> G.list range genNonInitial
        range = R.linear 0 100
        make i ns = Id (C.cons i ns)
