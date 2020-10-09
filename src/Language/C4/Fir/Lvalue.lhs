Lvalues and addresses
=====================

> {-# LANGUAGE DeriveFunctor, TemplateHaskell, TypeFamilies #-}

> {-|
> Module      : Language.C4.Fir.Lvalue
> Description : C4 Fuzzable Internal Representation: Lvalues and addresses
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.Lvalue
>   ( Address (Lv, Ref)
>   , Lvalue (Var, Deref)
>     -- Normalised addresses
>   , NormAddress
>   , normalise -- :: Address -> NormAddress
>     -- Recursion schemes
>   , AddressF (LvF, RefF)
>     -- Optics
>   , _Lv
>   , _Ref
>   , _Var
>   , _Deref
>   )
> where
> import qualified Control.Lens as LL
> import qualified Data.Functor.Foldable as F
> import Language.C4.Fir.Id (Id, AsId, _Id, HasId, underlyingId)

Lvalues
-------

> -- | Lvalues.
> data Lvalue
>   = Var Id -- ^ A variable reference.
>   | Deref Lvalue -- ^ A dereference of another lvalue.
>     deriving (Eq, Show)
> LL.makePrisms ''Lvalue

An lvalue can directly contain an identifier, as follows:

> instance AsId Lvalue where _Id = _Var

For recursion, we define a base functor.

> data LvalueF b
>   = VarF Id
>   | DerefF b
>     deriving Functor
>
> type instance F.Base Lvalue = LvalueF
> instance F.Recursive Lvalue where
>   project (Var v) = VarF v
>   project (Deref d) = DerefF d
> instance F.Corecursive Lvalue where
>   embed (VarF v) = Var v
>   embed (DerefF d) = Deref d


Once we have a working recursion scheme for lvalues, we define a method
of getting the underlying identifier of an lvalue.

> lvalueId :: LL.Lens' Lvalue Id
> lvalueId f = F.fold lvalueId'
>   where
>     lvalueId' (VarF x) = Var <$> f x
>     lvalueId' (DerefF x) = Deref <$> x

 > instance HasId Lvalue where underlyingId = lvalueId

Addresses
---------

> -- | Addresses.
> data Address
>   = Lv Lvalue -- ^ An lvalue as an address.
>   | Ref Address -- ^ A reference to another address.
>     deriving (Eq, Show)
> LL.makePrisms ''Address

An address is an identifier if it is `Lv (Var x)` for some `x`.

> instance AsId Address where _Id = _Lv . _Var

As with addresses, we define a base functor.

> data AddressF b
>   = LvF Lvalue
>   | RefF b
>     deriving Functor
>
> type instance F.Base Address = AddressF
> instance F.Recursive Address where
>   project (Lv l) = LvF l
>   project (Ref r) = RefF r
> instance F.Corecursive Address where
>   embed (LvF l) = Lv l
>   embed (RefF r) = Ref r

We could define `HasId` by composing a retrieval of the lvalue of an address
and the variable of the lvalue, but this seems slightly more optimal:

 > addressId :: Lvalue -> Id
 > addressId = F.fold addressId'
 >   where addressId' (LvF x) = lvalueId x
 >         addressId' (DerefF x) = x
 > instance HasId Address where underlyingId = addressId

Normalised addresses
--------------------

> -- | Normalised addresses.
> newtype NormAddress = Norm Address
>   deriving (Eq, Show)

Normalising an address involves eliminating any references of dereferences.
We do so using a fold where we progressively normalise up the address, starting
at its underlying lvalue.

> normalise :: Address -> NormAddress
> normalise = F.fold normalise'
>   where

A raw lvalue is already normalised.

>     normalise' (LvF x) = Norm (Lv x)

A reference of a normalised dereference, ie `&*x`, must be normalised to `x`.

>     normalise' (RefF (Norm (Lv (Deref x)))) = Norm (Lv x)

Any other reference of a normalised value is normalised.

>     normalise' (RefF (Norm x)) = Norm (Ref (Ref x))

Recursion schemes
-----------------
