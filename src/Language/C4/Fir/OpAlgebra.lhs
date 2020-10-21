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

> {-# LANGUAGE TemplateHaskell #-}

> {-|
> Module      : Language.C4.Fir.OpAlgebra
> Description : C4 Fuzzable Internal Representation: Operator algebraic rules
> Copyright   : (c) Matt Windsor, 2018, 2019, 2020
> License     : MIT
> Maintainer  : mattwindsor91@gmail.com
> Stability   : experimental
> -}
> module Language.C4.Fir.OpAlgebra
>   ( Term (X, K)
>   , RuleSet
>     -- Rule sets
>   , arithRules   -- :: RuleSet ABop (Term, Term)
>   , bitwiseRules -- :: RuleSet BBop (Term, Term)
>   , logicalRules -- :: RuleSet LBop (Term, Term)
>   , relRules     -- :: RuleSet RBop (Term, Term)
>   )
> where
> import Data.Function ((&))
> import qualified Data.Map as Map
> import Control.Lens
>   ( (^..)
>   , Iso'
>   , each
>   , iconcatMapOf
>   , ifolded
>   , iso
>   , ix
>   , makePrisms
>   , over
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

> instance K.AsConst Term where
>   _Const = _K

Inputs
------

Usually, we handle inputs in terms of lists of inputs (for unary operators)
and input pairs (for binary operators).

> -- | Type of input specification for binary inputs.
> type BInSpec = [(Term, Term)]

We define a few standard functions for expressing common input specifications.
These return lists and take position lists, because that makes
combinators like `anyPos` work better.

> -- | l `op` r is convenient shorthand for a position-dependent input spec.
> op :: Term -> Term -> BInSpec
> op l r = [(l, r)]

> -- | Appends the flipped version of an input specification.
> comm :: BInSpec -> BInSpec
> comm = (>>= commOne)
>   where
>     commOne (l, r) | l == r = [(l, r)]
>     commOne (l, r) = [(l, r), (r, l)]

> -- | Expands to the rules (t op X) and (X op t) where t is the input term.
> eitherSide :: Term -> BInSpec
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
> _rule :: Iso' (Rule o i) (Term, (Head o i))
> _rule = iso fromRule toRule
>   where fromRule (h :-> t) = (t, h)
>         toRule (t, h) = (h :-> t)

> -- | Views a rule list as a list of term-head pairs.
> ruleAssoc :: [Rule o i] -> [(Term, Head o i)]
> ruleAssoc = toListOf (each . _rule)

The weird shape of the `@->` operator exists to set up the DSL later on:
it lets us write `[ operators ] & input @-> output`.

> -- | Takes an input list and output term, and produces a rule stub.
> (@->) :: Ord o => [i] -> Term -> [o] -> Rule o i
> (@->) ins out ops = singleHead ops ins :-> out

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

Reflexivity is a common rule:

> -- | Encodes a reflexivity rule.
> refl :: Ord o => [o] -> Rule o (Term, Term)
> refl ops = ops & X `op` X @-> X

Rule tables
-----------

We now have the actual rule tables for each operator.

> -- | Rule table for arithmetic binary operators.
> arithRules :: RuleSet Op.ABop (Term, Term)
> arithRules = rules
>   [ [(Op.:+)] & eitherSide K.zero @-> X
>   , [(Op.:-)] & X `op` K.zero     @-> X 
>   , [(Op.:-)] & X `op` X          @-> K.zero
>   ]

> -- | Rule table for bitwise binary operators.
> bitwiseRules :: RuleSet Op.BBop (Term, Term)
> bitwiseRules = rules
>   [ [(Op.:&), (Op.:|)] & refl
>   , [(Op.:&)]          & eitherSide K.zero @-> K.zero
>   , [(Op.:|), (Op.:^)] & eitherSide K.zero @-> X
>   , [(Op.:^)]          & X `op` X          @-> K.zero
>   ]

> -- | Rule table for logical binary operators.
> logicalRules :: RuleSet Op.LBop (Term, Term)
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
> relRules :: RuleSet Op.RBop (Term, Term)
> relRules = rules (map ruleFor (enumFrom minBound))
>   where
>     ruleFor :: Op.RBop -> Rule Op.RBop (Term, Term)
>     ruleFor o = [o] & X `op` X @-> K.bool (Op.relIncl o (Op.:==))

Using rule tables
-----------------

We don't expose the internal representation of the rule tables, to let us
further optimise them later.  Instead, we have a few combinators for looking
up rules.

> inputsFor :: RuleSet o i -> Term -> [(o, i)]
> inputsFor (RuleSet tm) inp = iconcatMapOf (ix inp . _Head . ifolded) zip tm

