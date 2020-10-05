Identifiers
===========

> {-|
> Module      : Language.C4.Fir.Id
> Description : C4 Fuzzable Internal Representation: Identifiers
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.Id
>   ( Id
>   , bytes -- :: Control.Lens.Prism' Data.ByteString.Char8.ByteString Id
>   , gen -- :: Hedgehog.Gen.MonadGen m => m Id
>   )
> where
> import Data.Char (isAlpha, isAlphaNum)
> import qualified Control.Lens.Prism as LP
> import qualified Data.ByteString.Char8 as C

We import `Hedgehog` to provide generators for identifiers.

> import Hedgehog (MonadGen)
> import qualified Hedgehog.Gen as G
> import qualified Hedgehog.Range as R

Invariants
----------

The `isValidInitial` and `isValidNonInitial` predicates capture the lexing rules
of C identifiers: each character can be an ASCII alphanumeric character or
underscore, and the first character must not be a number.

Correspondingly, `genInitial` and `genNonInitial` provide Hedgehog generators
that obey their respective predicates.

> isValidInitial :: Char -> Bool
> isValidInitial c = isAlpha c || c == '_'

> genInitial :: MonadGen m => m Char
> genInitial = G.choice [ G.alpha, return '_' ]

> isValidNonInitial :: Char -> Bool
> isValidNonInitial c = isAlphaNum c || c == '_'

> genNonInitial :: MonadGen m => m Char
> genNonInitial = G.choice [ G.alphaNum, return '_' ]

The Id type
-----------

Since we want to enforce C's identifier invariants, we don't expose the internal
type of identifiers, but they are bytestrings.

> -- | Type of FIR identifiers.
> newtype Id = Id C.ByteString
>            deriving (Eq, Show)

We can convert an identifier to a bytestring just by removing its wrapper.

> toBytes :: Id -> C.ByteString
> toBytes (Id x) = x

Converting back is partial, because incoming bytestrings must obey the above
invariants.

> fromBytes :: C.ByteString -> Maybe Id
> fromBytes x = C.uncons x >>= fromBytes'
>   where
>     fromBytes' (c, cs) | isValidInitial c && C.all isValidNonInitial cs = Just (Id x)
>     fromBytes' _ = Nothing

There then exists a prism mapping from bytestrings to identifiers: all
identifiers are valid bytestrings, but not all bytestrings are valid
identifiers.

> -- | Lets us view valid C identifier bytestrings as `Id`s, and vice versa.
> bytes :: LP.Prism' C.ByteString Id
> bytes = LP.prism' toBytes fromBytes

Generating IDs
--------------

> -- | Generates random, non-human-readable valid identifiers.
> gen :: MonadGen m => m Id
> gen = make <$> genInitial <*> genNonInitials
>   where genNonInitials = C.pack <$> G.list range genNonInitial
>         range = R.linearBounded
>         make i ns = Id (C.cons i ns)


