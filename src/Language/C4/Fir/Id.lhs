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
>     -- Conversion to and from bytestrings
>   , fromBytes -- :: Data.ByteString.Char8.ByteString -> Maybe Id
>   , toBytes -- :: Id -> Data.ByteString.Char8.ByteString
>     -- Optics
>   , AsId -- typeclass
>   , _Id -- :: AsId t => Control.Lens.Prism' t Id
>   , bytes -- :: Control.Lens.Prism' Data.ByteString.Char8.ByteString Id
>   , HasId -- :: typeclass
>   , underlyingId -- :: HasId t => Control.Lens.Lens' t Id
>     -- Generators
>   , gen -- :: Hedgehog.Gen.MonadGen m => m Id
>     -- Miscellaneous
>   , isValid -- :: Data.ByteString.Char8.ByteString -> bool
>   )
> where
> import Data.Char (isAlpha, isAlphaNum)
> import qualified Control.Lens as LL
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

> -- | Checks whether a bytestring is a valid identifier.
> isValid :: C.ByteString -> Bool
> isValid = any isValid' . C.uncons
>   where
>     isValid' (c, cs) = isValidInitial c && C.all isValidNonInitial cs

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
invariant.

> fromBytes :: C.ByteString -> Maybe Id
> fromBytes x = if isValid x then Just (Id x) else Nothing

Optics
------

We generate two sets of 'classy' optics for identifiers:

- a prism for focusing on things that can _directly_ hold an identifier:
  `AsId`;
- a lens for focusing on the underlying identifier of things that _absolutely_
  hold one, if indirectly: `HasId`.

Deriving the two using the usual Template Haskell schemes doesn't seem to work
well, but we can do so manually.

> -- | Typeclass of types that can directly represent identifiers.
> class AsId t where
>   _Id :: LP.Prism' t Id -- ^ Tries to view the type as an Id.
> instance AsId Id where _Id = id

> -- | Typeclass of types that can indirectly hold identifiers.
> class HasId t where
>   underlyingId :: LL.Lens' t Id -- ^ Focuses on the underlying identifier.
> instance HasId Id where underlyingId = id

There exists a prism mapping from bytestrings to identifiers: all
identifiers are valid bytestrings, but not all bytestrings are valid
identifiers.

> -- | Lets us view valid C identifier bytestrings as `Id`s, and vice versa.
> bytes :: LP.Prism' C.ByteString Id
> bytes = LP.prism' toBytes fromBytes

> instance AsId C.ByteString where _Id = bytes

Generating IDs
--------------

> -- | Generates random, non-human-readable valid identifiers.
> gen :: MonadGen m => m Id
> gen = make <$> genInitial <*> genNonInitials
>   where genNonInitials = C.pack <$> G.list range genNonInitial
>         range = R.linear 0 100
>         make i ns = Id (C.cons i ns)


