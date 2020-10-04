Expressions
-----------

> module Language.C4.Fir.Expr
>   ( Expr ()
>   , Const (Int, Bool)
>   , Id
>   ,
>   )
> where
> import Language.C4.Fir.Id (Id)
> import qualified Control.Lens.Prism as LP
> import qualified Data.ByteString.Char8 as C

Constants
=========

Fir supports (signed) integer and Boolean ("true", "false") constants.

> data Const = Int Integer
>            | Bool Bool
>            deriving (Eq, Show)

Expressions
===========

> data Prim = Constant Const
>           | Ident Id
>           deriving (Eq, Show)

> data Expr m = Meta m (Expr m)
>             | Prim
>             deriving (Eq, Show)
