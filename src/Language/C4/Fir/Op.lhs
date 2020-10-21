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
>     -- Unary operators
>   , Uop (Comp, Not)
>     -- Predicates
>   , relIncl -- :: RBop -> RBop -> Bool
>     -- Semantics
>   , semABop -- :: ABop -> Int -> Int -> Int
>   , semBBop -- :: BBop -> Int -> Int -> Int
>   , semLBop -- :: Monad m => LBop -> m Bool -> m Bool -> m Bool
>   , semRBop -- :: (Eq a, Ord a) -> RBop -> a -> a -> Bool
>   )
> where
> import Data.Bits ((.&.), (.|.), xor)

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

With all of the operator groupings, the order is arbitrary and only meaningful
for situations such as building operator maps.

Arithmetic operators take integers and produce integers.

> -- | Arithmetic binary operators.
> data ABop
>   = (:+) -- ^ Addition.
>   | (:-) -- ^ Subtraction.
>     deriving (Eq, Ord, Show, Bounded, Enum)

The semantics is as follows:

> semABop :: ABop -> Int -> Int -> Int
> semABop (:+) = (+)
> semABop (:-) = (-)

Bitwise operators also take integers and produce integers, but operate on the
bits of their operands.

> -- | Bitwise binary operators.
> data BBop
>   = (:&) -- ^ Bitwise AND.
>   | (:|) -- ^ Bitwise OR.
>   | (:^) -- ^ Bitwise XOR.
>     deriving (Eq, Ord, Show, Bounded, Enum)

The semantics is as follows:

> semBBop :: BBop -> Int -> Int -> Int
> semBBop (:&) = (.&.)
> semBBop (:|) = (.|.)
> semBBop (:^) = xor

Logical operators take Booleans and produce Booleans.

> -- | Logical binary operators.
> data LBop
>   = (:&&) -- ^ Conjunction.
>   | (:||) -- ^ Disjunction.
>     deriving (Eq, Ord, Show, Bounded, Enum)

The semantics is as follows.  We parametrise the semantics over a monad, and
this lets us capture the potential of short-circuiting evaluation.

> semLBop :: Monad m => LBop -> m Bool -> m Bool -> m Bool
> semLBop (:&&) l r =
>   do
>     l' <- l
>     if l' then (l' &&) <$> r else pure False
> semLBop (:||) l r =
>   do
>     l' <- l
>     if l' then pure True else (l' ||) <$> r

Relational operators take two operands of the same type, and produce Booleans.

> -- | Relational binary operators.
> data RBop
>   = (:<) -- ^ Less-than.
>   | (:<=) -- ^ Less-than-or-equal.
>   | (:>) -- ^ Greater-than.
>   | (:>=) -- ^ Greater-than-or-equal.
>   | (:==) -- ^ Equal.
>   | (:!=) -- ^ Not-equal.
>     deriving (Eq, Ord, Show, Bounded, Enum)

The semantics is as follows.  Unlike most semantic functions, relationals are
parametric over the type, so long as it is ordered with equality.

> semRBop :: (Eq a, Ord a) => RBop -> a -> a -> Bool
> semRBop (:<) = (<)
> semRBop (:<=) = (<=)
> semRBop (:>) = (>)
> semRBop (:>=) = (>=)
> semRBop (:==) = (==)
> semRBop (:!=) = (/=)

> {- | True if, and only if, the LHS relation includes the RHS relation. -}
> relIncl :: RBop -> RBop -> Bool

Reflexive cases first:

> relIncl (:<) (:<) = True
> relIncl (:<=) (:<=) = True
> relIncl (:>) (:>) = True
> relIncl (:>=) (:>=) = True
> relIncl (:==) (:==) = True
> relIncl (:!=) (:!=) = True

These operators are composites of other relations:

> relIncl (:<=) (:<) = True
> relIncl (:<=) (:==) = True
> relIncl (:>=) (:>) = True
> relIncl (:>=) (:==) = True

Other cases are invalid:

> relIncl _ _ = False

Unary operators
---------------

There aren't enough unary operators to benefit from grouping, so we just have
one linear enumeration.

> -- Enumeration of all unary operators.
> data Uop
>   = Comp -- ^ Bitwise complement.
>   | Not -- ^ Logical negation.
>     deriving (Eq, Ord, Show, Bounded, Enum)

We don't give the semantics of Uop here, as there are only two operators and
they take different types.
