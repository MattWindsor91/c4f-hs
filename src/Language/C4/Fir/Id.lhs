Identifiers
-----------

> module Language.C4.Fir.Id
>   ( Id
>   , bytes -- :: Control.Lens.Prism' Data.ByteString.Char8.ByteString Id
>   )
> where
> import Data.Char (isAlpha, isAlphaNum)
> import qualified Control.Lens.Prism as LP
> import qualified Data.ByteString.Char8 as C

Invariants of identifiers
=========================

The `isValidInitial` and `isValidNonInitial` functions capture the lexing rules
of C identifiers: each character can be an ASCII alphanumeric character or
underscore, and the first character must not be a number.

> isValidInitial :: Char -> Bool
> isValidInitial c = isAlpha c || c == '_'

> isValidNonInitial :: Char -> Bool
> isValidNonInitial c = isAlphaNum c || c == '_'

The Id type
===========

Since we want to enforce C's identifier invariants, we don't expose the internal
type of identifiers, but they are bytestrings.

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

> bytes :: LP.Prism' C.ByteString Id
> bytes = LP.prism' toBytes fromBytes


