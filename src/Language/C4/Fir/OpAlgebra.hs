{-# LANGUAGE DeriveTraversable
           , TemplateHaskell
           , TupleSections
           , NamedFieldPuns #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.OpAlgebra
-- Description : C4 Fuzzable Internal Representation: Operator algebraic rules
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- A key component of the C4F expression generator is the ability to use
-- particular algebraic rules over operators.  We encode these rules
-- programmatically in this module.
-- 
-- Each rule is of the form `forall x. in(x) -> out(x)`, where `x` is some
-- equivalence class on expressions.  For binary operators, `in(x)` will
-- usually be of the form `x OP x` (two equivalent expressions), `k OP x` (left
-- is a constant), or `x OP k` (right is a constant).  The output `out(x)` is
-- either `x` or `k`.
--------------------------------------------------------------------------------
module Language.C4.Fir.OpAlgebra
  ( -- Terms
    Term (X, K)
  , subst
  , fill
    -- Input specifications
  , BIn (BIn, lhs, rhs)
  , onBIn
    -- Rule sets
  , RuleSet
  , arithRules
  , bitwiseRules
  , logicalRules
  , relRules
    -- Rule operations
  , allRules
  , rulesForOutput
  ) where

import Control.Applicative (liftA2, liftA3)
import Data.Function ((&))
import Data.Traversable (mapAccumL)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import Control.Lens
  ( (<.>)
  , (#) -- review
  , Iso'
  , each
  , ifolded
  , ifoldMapOf
  , iso
  , ix
  , makePrisms
  , toListOf
  )
import qualified Language.C4.Fir.Const as K
import qualified Language.C4.Fir.Op as Op

-- | Encoding of terms in rules.
data Term
  = X -- ^ This term is a member of the input expression equivalence class.
  | K K.Const -- ^ This term is a constant.
    deriving (Eq, Ord, Show)
makePrisms ''Term

{- Implementing `AsConst` here gives us free access to various convenience
   constructors. -}
instance K.AsConst Term where _Const = _K

-- | Substitutes an expression into a term if it is X;
--   otherwise, evaluates to its constant as an expression.
subst :: K.AsConst x => x -> Term -> x
subst x X     = x
subst _ (K k) = K._Const#k

-- | Type of input specification for binary inputs.
--
-- While 'Term' and `subst' is useful for dealing with inputs to unary
-- operators, binary operators take pairs of inputs, and we need to be able to
-- substitute across both simultaneously (with one or more input expressions).
data BIn a =
  BIn
    { lhs :: a -- ^ The left-hand side of the input specification.
    , rhs :: a -- ^ The right-hand side of the input specification.
    } deriving (Functor, Foldable, Traversable, Show)

-- | Lifts a function on two inputs to one over an input specification.
--
--   This is effectively a fancy version of 'uncurry'.
onBIn :: (a -> a -> b) -- ^ The function to receive the input specification.
      -> BIn a         -- ^ The input specification to apply.
      -> b             -- ^ The result of the function.
onBIn f BIn {lhs, rhs} = f lhs rhs

-- | Fills an input specification, replacing each X-term with an expression
--   in the given list.  The i-th expression replaces, if appropriate, the
--   i-th term, with the list being cycled infinitely if needed.
fill :: (K.AsConst x, Traversable t) => NE.NonEmpty x -> t Term -> t x
fill inputs = snd . mapAccumL fillStep (NE.cycle inputs)
  -- Since the input list is cycled infinitely, `fromList` should be safe here
  where fillStep (x NE.:| xs) = (NE.fromList xs ,) . subst x

-- | l `op` r is convenient shorthand for a position-dependent input spec.
op :: Term -> Term -> [BIn Term]
op lhs rhs = [BIn {lhs, rhs}]

-- | Appends the flipped version of an input specification.
comm :: [BIn Term] -> [BIn Term]
comm = (>>= commOne)
  where
    commOne BIn {lhs, rhs}
      | lhs == rhs = lhs `op` rhs
      | otherwise  = [ BIn {lhs, rhs}, BIn {lhs=rhs, rhs=lhs} ]

-- | Expands to the rules (t op X) and (X op t) where t is the input term.
eitherSide :: Term -> [BIn Term]
eitherSide = comm . (X `op`)

-- | The head of a rule.
--
-- We don't expose the heads directly, but they form maps for ease of merging.
newtype Head o i = Head (Map.Map [o] [i])
makePrisms ''Head

-- | Makes a singleton head given a list of outputs and list of inputs.
singleHead :: Ord o => [o] -> [i] -> Head o i
singleHead = (Head .) . Map.singleton

-- | Merges two rule heads.
mergeHeads :: Ord o => Head o i -> Head o i -> Head o i
{- When we merge two heads, we're taking two lists of inputs that hold on the
   same list of operators, and combining them.  We don't bother to try to
   compact down other head redundancies (such as merging `[x1] -> [y]` and
   `[x2] -> [y]` to `[x1, x2] -> y`) yet, as this would involve quite a bit of
   repeated walking up and down the map. -}
mergeHeads (Head l) (Head r) = Head (Map.unionWith (++) l r)

-- | A single algebraic rule, with multiple disjunctive heads and one output.
data Rule o i = (:->) (Head o i) Term

-- | A rule can be seen as a term-head pair.
_rule :: Iso' (Rule o i) (Term, Head o i)
_rule = iso fromRule toRule
  where fromRule (h :-> t) = (t, h)
        toRule (t, h) = h :-> t

-- | Views a rule list as a list of term-head pairs.
ruleAssoc :: [Rule o i] -> [(Term, Head o i)]
ruleAssoc = toListOf (each . _rule)

-- | Takes an input list and output term, and produces a rule stub.
--
-- The weird shape of the '@->' operator exists to set up the DSL later on:
-- it lets us write `[ operators ] & input @-> output`.
(@->) :: Ord o => [i] -> Term -> [o] -> Rule o i
(@->) ins out ops = singleHead ops ins :-> out

-- | Encodes a reflexivity rule.
refl :: Ord o => [o] -> Rule o (BIn Term)
refl ops = ops & X `op` X @-> X

-- | A set of rules.
newtype RuleSet o i =
  {- Since the most common use of rule sets is to look up a list of possible
     inputs given a desired output, we represent them internally as maps from
     terms to heads.  This is an implementation detail, so we hide it behind a
     newtype and provide a smart constructor. -}
  RuleSet (Map.Map Term (Head o i))

-- | Lifts a rule list to a rule set.
rules :: Ord o => [Rule o i] -> RuleSet o i
rules = RuleSet . Map.fromListWith mergeHeads . ruleAssoc

-- | Given a ruleset and a desired output, get the list of operator-input
--   pairs that result in that output.
rulesForOutput :: RuleSet o i -> Term -> [(o, i)]
{- Since we want every pair of operator and input specification, we need a
   Cartesian product, not a zip. -}
rulesForOutput (RuleSet tm) inp = ifoldMapOf fold prod2 tm
  where fold = ix inp . _Head . ifolded

-- | Given a ruleset, get the list of operator-input-output triples it encodes.
allRules :: RuleSet o i -> [(o, i, Term)]
allRules (RuleSet tm) = ifoldMapOf (ifolded <.> _Head . ifolded) prodTerm tm
  where prodTerm (out, ops) ins = prod3 ops ins [out]

-- | Cartesian 2-product.
prod2 :: [a] -> [b] -> [(a, b)]
prod2 = liftA2 (,)

-- | Cartesian 3-product
prod3 :: [a] -> [b] -> [c] -> [(a, b, c)]
prod3 = liftA3 (,,)

--------------------------------------------------------------------------------
-- Rule tables
--------------------------------------------------------------------------------

-- | Rule table for arithmetic binary operators.
arithRules :: RuleSet Op.ABop (BIn Term)
arithRules = rules
  [ [(Op.:+)] & eitherSide K.zero @-> X
  , [(Op.:-)] & X `op` K.zero     @-> X 
  , [(Op.:-)] & X `op` X          @-> K.zero
  ]

-- | Rule table for bitwise binary operators.
bitwiseRules :: RuleSet Op.BBop (BIn Term)
bitwiseRules = rules
  [ [(Op.:&), (Op.:|)] & refl
  , [(Op.:&)]          & eitherSide K.zero       @-> K.zero
  , [(Op.:|), (Op.:^)] & eitherSide K.zero       @-> X
  , [(Op.:^)]          & X `op` X                @-> K.zero
{- This rule relies on twos-complement arithmetic to hold, which is a little
   nonstandard, but should hold for any target architecture our fuzzer output
   will build for. -}
  , [(Op.:&)]          & eitherSide (K.i32 (-1)) @-> X
  ]

-- | Rule table for logical binary operators.
logicalRules :: RuleSet Op.LBop (BIn Term)
logicalRules = rules
  [ [(Op.:&&), (Op.:||)] & refl
  , [(Op.:&&)]           & eitherSide K.false @-> K.false 
  , [(Op.:&&)]           & eitherSide K.true  @-> X
  , [(Op.:||)]           & eitherSide K.false @-> X
  , [(Op.:||)]           & eitherSide K.true  @-> K.true
  ]

-- | relRules is the rule table for relational operators.
--
-- Relational rules boil down, for now, to reflexivity: any relation that
-- implies `==` has the rule `x OP x == true`; the others, `x OP x == false`.
relRules :: RuleSet Op.RBop (BIn Term)
relRules = rules (map ruleFor (enumFrom minBound))
  where
    ruleFor :: Op.RBop -> Rule Op.RBop (BIn Term)
    ruleFor o = [o] & X `op` X @-> K.bool (Op.relIncl o (Op.:==))


