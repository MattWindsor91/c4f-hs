Algebraic rules over operators
==============================

A key component of the C4F expression generator is the ability to use particular
algebraic rules over operators.  We encode these rules programmatically
in this module.

Each rule is of the form `forall x. in(x) -> out(x)`, where `x` is some
equivalence class on expressions.  For binary operators, `in(x)` will usually
be of the form `x OP x` (two equivalent expressions), `k OP x` (left is a
constant), or `x OP k` (right is a constant).  The output `out(x)` is either
`x` or `k`.

> {-# LANGUAGE DeriveFoldable
>            , DeriveFunctor
>            , DeriveTraversable
>            , TemplateHaskell
>            , TupleSections
>            , NamedFieldPuns #-}

> {-|
> Module      : Language.C4.Fir.OpAlgebra
> Description : C4 Fuzzable Internal Representation: Operator algebraic rules
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.OpAlgebra
>   ( -- Terms
>     Term (X, K)
>   , subst          -- :: K.AsConst x => x -> Term -> x
>   , fill           -- :: (K.AsConst x, Traversable t) => NonEmpty x -> t Term -> t x
>     -- Rule sets
>   , RuleSet
>   , arithRules     -- :: RuleSet ABop (Term, Term)
>   , bitwiseRules   -- :: RuleSet BBop (Term, Term)
>   , logicalRules   -- :: RuleSet LBop (Term, Term)
>   , relRules       -- :: RuleSet RBop (Term, Term)
>     -- Rule operations
>   , allRules       -- :: RuleSet o i -> [(o, i, Term)]
>   , rulesForOutput -- :: RuleSet o i -> Term -> [(o, i)]
>   )
> where
> import Control.Applicative (liftA2, liftA3)
> import Data.Function ((&))
> import Data.Traversable (mapAccumL)
> import qualified Data.Map as Map
> import qualified Data.List.NonEmpty as NE
> import Control.Lens
>   ( (<.>)
>   , (#) -- review
>   , Iso'
>   , each
>   , ifolded
>   , ifoldMapOf
>   , iso
>   , ix
>   , makePrisms
>   , toListOf
>   )
> import qualified Language.C4.Fir.Const as K
> import qualified Language.C4.Fir.Op as Op

Terms
-----

> -- | Encoding of terms in rules.
> data Term
>   = X -- ^ This term is a member of the input expression equivalence class.
>   | K K.Const -- ^ This term is a constant.
>     deriving (Eq, Ord)
> makePrisms ''Term

Implementing `AsConst` here gives us free access to various convenience
constructors.

> instance K.AsConst Term where _Const = _K

When we have `Term`s in a rule, we generally need to eliminate them by
substituting expressions for `X` terms.  The `subst` function handles this:

> -- | Substitutes an expression into a term if it is X;
> --   otherwise, evaluates to its constant as an expression.
> subst :: K.AsConst x => x -> Term -> x
> subst x X     = x
> subst _ (K k) = K._Const#k

Binary input specifications
---------------------------

While `Term` and `subst` is useful for dealing with inputs to unary operators,
binary operators take pairs of inputs, and we need to be able to substitute
across both simultaneously (with one or more 

> -- | Type of input specification for binary inputs.
> data BIn a =
>   BIn
>     { lhs :: a -- ^ The left-hand side of the input specification.
>     , rhs :: a -- ^ The right-hand side of the input specification.
>     }
>   deriving (Functor, Foldable, Traversable)

If we have an input 

> -- | Fills an input specification, replacing each X-term with an expression
> --   in the given list.  The i-th expression replaces, if appropriate, the
> --   i-th term, with the list being cycled infinitely if needed.
> fill :: (K.AsConst x, Traversable t) => NE.NonEmpty x -> t Term -> t x

Since the input list is cycled infinitely, `fromList` should be safe here:

> fill inputs = snd . mapAccumL fillStep (NE.cycle inputs)
>   where fillStep (x NE.:| xs) = (NE.fromList xs ,) . subst x

We define a few standard functions for expressing common input specifications.
These return lists and take position lists, because that makes
combinators like `anyPos` work better.

> -- | l `op` r is convenient shorthand for a position-dependent input spec.
> op :: Term -> Term -> [BIn Term]
> op lhs rhs = [BIn {lhs, rhs}]

> -- | Appends the flipped version of an input specification.
> comm :: [BIn Term] -> [BIn Term]
> comm = (>>= commOne)
>   where
>     commOne (BIn {lhs, rhs})
>       | lhs == rhs = lhs `op` rhs
>       | otherwise  = [ BIn {lhs, rhs}, BIn {lhs=rhs, rhs=lhs} ]

> -- | Expands to the rules (t op X) and (X op t) where t is the input term.
> eitherSide :: Term -> [BIn Term]
> eitherSide = comm . (X `op`)

Rules
-----

A rule is a relation between an output and zero or more 'heads', each of which
specifies a choice of operators and a choice of inputs.

We don't expose the heads directly, but they form maps for ease of merging.

> -- | The head of a rule.
> newtype Head o i = Head (Map.Map [o] [i])
> makePrisms ''Head

> -- | Makes a singleton head given a list of outputs and list of inputs.
> singleHead :: Ord o => [o] -> [i] -> Head o i
> singleHead = (Head .) . Map.singleton

When we merge two heads, we're taking two lists of inputs that hold on the
same list of operators, and combining them.  We don't bother to try to compact
down other head redundancies (such as merging `[x1] -> [y]` and `[x2] -> [y]`
to `[x1, x2] -> y`) yet, as this would involve quite a bit of repeated walking
up and dowh map.

> -- | Merges two rule heads.
> mergeHeads :: Ord o => Head o i -> Head o i -> Head o i
> mergeHeads (Head l) (Head r) = Head (Map.unionWith (++) l r)

> -- | A single algebraic rule, with multiple disjunctive heads and one output.
> data Rule o i = (:->) (Head o i) Term

> -- | A rule can be seen as a term-head pair.
> _rule :: Iso' (Rule o i) (Term, Head o i)
> _rule = iso fromRule toRule
>   where fromRule (h :-> t) = (t, h)
>         toRule (t, h) = h :-> t

> -- | Views a rule list as a list of term-head pairs.
> ruleAssoc :: [Rule o i] -> [(Term, Head o i)]
> ruleAssoc = toListOf (each . _rule)

The weird shape of the `@->` operator exists to set up the DSL later on:
it lets us write `[ operators ] & input @-> output`.

> -- | Takes an input list and output term, and produces a rule stub.
> (@->) :: Ord o => [i] -> Term -> [o] -> Rule o i
> (@->) ins out ops = singleHead ops ins :-> out

This is how we encode the common rule of reflexivity:

> -- | Encodes a reflexivity rule.
> refl :: Ord o => [o] -> Rule o (BIn Term)
> refl ops = ops & X `op` X @-> X

Rule sets
---------

Since the most common use of rule sets is to look up a list of possible inputs
given a desired output, we represent them internally as maps from terms to
heads.  This is an implementation detail, so we hide it behind a newtype and
provide a smart constructor.

> -- | A set of rules.
> newtype RuleSet o i = RuleSet (Map.Map Term (Head o i))

> rules :: Ord o => [Rule o i] -> RuleSet o i
> rules = RuleSet . Map.fromListWith mergeHeads . ruleAssoc

Rule tables
-----------

We now have the actual rule tables for each operator.

> -- | Rule table for arithmetic binary operators.
> arithRules :: RuleSet Op.ABop (BIn Term)
> arithRules = rules
>   [ [(Op.:+)] & eitherSide K.zero @-> X
>   , [(Op.:-)] & X `op` K.zero     @-> X 
>   , [(Op.:-)] & X `op` X          @-> K.zero
>   ]

> -- | Rule table for bitwise binary operators.
> bitwiseRules :: RuleSet Op.BBop (BIn Term)
> bitwiseRules = rules
>   [ [(Op.:&), (Op.:|)] & refl
>   , [(Op.:&)]          & eitherSide K.zero       @-> K.zero
>   , [(Op.:|), (Op.:^)] & eitherSide K.zero       @-> X
>   , [(Op.:^)]          & X `op` X                @-> K.zero

This rule relies on twos-complement arithmetic to hold, which is a little
nonstandard, but should hold for any target architecture our fuzzer output will
build for.

>   , [(Op.:&)]          & eitherSide (K.i32 (-1)) @-> X
>   ]

> -- | Rule table for logical binary operators.
> logicalRules :: RuleSet Op.LBop (BIn Term)
> logicalRules = rules
>   [ [(Op.:&&), (Op.:||)] & refl
>   , [(Op.:&&)]           & eitherSide K.false @-> K.false 
>   , [(Op.:&&)]           & eitherSide K.true  @-> X
>   , [(Op.:||)]           & eitherSide K.false @-> X
>   , [(Op.:||)]           & eitherSide K.true  @-> K.true
>   ]

Relational rules boil down, for now, to reflexivity: any relation that implies
`==` has the rule `x OP x == true`; the others, `x OP x == false`.

> -- | relRules is the rule table for relational operators.
> relRules :: RuleSet Op.RBop (BIn Term)
> relRules = rules (map ruleFor (enumFrom minBound))
>   where
>     ruleFor :: Op.RBop -> Rule Op.RBop (BIn Term)
>     ruleFor o = [o] & X `op` X @-> K.bool (Op.relIncl o (Op.:==))

Using rule tables
-----------------

We don't expose the internal representation of the rule tables, to let us
further optimise them later.  Instead, we have a few combinators for looking
up rules.

The usual thing to do with a ruleset is to call up the entire list of possible
operators and input specifications.

> -- | Given a ruleset and a desired output, get the list of operator-input
> --   terms that result in that output.
> rulesForOutput :: RuleSet o i -> Term -> [(o, i)]
> rulesForOutput (RuleSet tm) inp = ifoldMapOf fold prod tm
>   where fold = ix inp . _Head . ifolded

Since we want every pair of operator and input specification, we need a
Cartesian product, not a zip.  Here is a Cartesian product (for lack of
better library definition we can use):

> -- | Cartesian product.
> prod :: [a] -> [b] -> [(a, b)]
> prod = liftA2 (,)

Sometimes (usually for testing) we want to get every rule out of a rule set.
This effectively involves turning the internal representation of the set back
into lists.

> allRules :: RuleSet o i -> [(o, i, Term)]

`allRules` is similar to `rulesForOutput`, except that, instead of taking one
term and using it to index into the outer map, we pull the indices of the
outer map into the fold over the inner maps using lens magic, and adjust the
product accordingly.

> allRules (RuleSet tm) = ifoldMapOf (ifolded <.> _Head . ifolded) prodTerm tm
>   where prodTerm (out, ops) ins = liftA3 (,,) ops ins [out]
