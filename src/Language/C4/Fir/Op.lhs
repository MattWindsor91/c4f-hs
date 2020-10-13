Operators
=========

> {-|
> Module      : Language.C4.Fir.Op
> Description : C4 Fuzzable Internal Representation: Operators
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.Op
>   ( -- Binary operators
>     ABop ((:+), (:-))
>   , BBop ((:&), (:|), (:^))
>   , LBop ((:&&), (:||))
>   , RBop ((:<), (:<=), (:>), (:>=), (:==), (:!=))
>   , Bop (Arith, Bitwise, Logical, Rel)
>   , Uop (Not)
>   )
> where

Binary operators
----------------

We group operators into four semantic groupings: arithmetic, bitwise, logical,
and relational.

> -- | Enumeration of all binary operators.
> data Bop
>   = Arith ABop -- ^ An arithmetic operator.
>   | Bitwise BBop -- ^ A bitwise operator.
>   | Logical LBop -- ^ A logical operator.
>   | Rel RBop -- ^ A relational operator.
>     deriving (Eq, Show)

Arithmetic operators take integers and produce integers.

> -- | Arithmetic binary operators.
> data ABop
>   = (:+) -- ^ Addition.
>   | (:-) -- ^ Subtraction.
>     deriving (Eq, Show)

Bitwise operators also take integers and produce integers, but operate on the
bits of their operands.

> -- | Bitwise binary operators.
> data BBop
>   = (:&) -- ^ Bitwise AND.
>   | (:|) -- ^ Bitwise OR.
>   | (:^) -- ^ Bitwise XOR.
>     deriving (Eq, Show)

Logical operators take Booleans and produce Booleans.

> -- | Logical binary operators.
> data LBop
>   = (:&&) -- ^ Conjunction.
>   | (:||) -- ^ Disjunction.
>     deriving (Eq, Show)

Relational operators take two operands of the same type, and produce Booleans.

> -- | Relational binary operators.
> data RBop
>   = (:<) -- ^ Less-than.
>   | (:<=) -- ^ Less-than-or-equal.
>   | (:>) -- ^ Greater-than.
>   | (:>=) -- ^ Greater-than-or-equal.
>   | (:==) -- ^ Equal.
>   | (:!=) -- ^ Not-equal.
>     deriving (Eq, Show)

Unary operators
---------------

There aren't enough unary operators to benefit from grouping, so we just have
one linear enumeration.

> -- Enumeration of all unary operators.
> data Uop
>   = Not -- ^ Logical negation.
>     deriving (Eq, Show)
