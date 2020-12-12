{-# LANGUAGE DeriveTraversable
           , FlexibleContexts
           , MultiParamTypeClasses
           , NamedFieldPuns
           , RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.If
-- Description : Conditional pairs of constructs in FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- Both conditional expressions and if statements have the notion of a pair of
-- constructs, one available when something is 'True', another when something
-- is 'False'.  This module contains a datatype and various instances for
-- capturing this.
--
-- 'If' captures a homogeneous pair that is indexed by a Boolean, with indexed
-- functor, foldable, traversable, and lens instances available.
--------------------------------------------------------------------------------

module Language.C4.Fir.If
  ( If (If, _tBranch, _fBranch)
    -- * Optics
  , branch
  , tBranch
  , fBranch
  , ifTuple
  ) where

import qualified Control.Lens as L

-- | A pair of items representing the branches of an if construct.
data If x
  = If
      { _tBranch :: x -- ^ The 'True' variant.
      , _fBranch :: x -- ^ The 'False' variant.
      } deriving ( Eq
                 , Show
                 , Functor
                 , Foldable
                 , Traversable
                 )

-- | Infix notation for if constructs.
--
--   The operator choice is meant to resemble the : bit of a conditional
--   expression.
(@:) :: x    -- ^ The 'True' variant.
     -> x    -- ^ The 'False' variant.
     -> If x -- ^ The if construct.
_tBranch @: _fBranch = If { _tBranch, _fBranch }

-- | An if construct is isomorphic to a homogeneous pair.
ifTuple :: L.Iso (If a) (If b) (a, a) (b, b)
ifTuple = L.iso tuple elput
  where tuple If { _tBranch, _fBranch } =    ( _tBranch, _fBranch )
        elput    ( _tBranch, _fBranch ) = If { _tBranch, _fBranch }

-- | Focuses on the true branch of an if construct.
--   The index, if used, will be 'True'.
tBranch :: L.IndexedLens' Bool (If x) x
tBranch f If { _tBranch, _fBranch } =
  (@: _fBranch) <$> L.indexed f True _tBranch

-- | Focuses on the true branch of an if construct.
--   The index, if used, will be 'False'.
fBranch :: L.IndexedLens' Bool (If x) x
fBranch f If { _fBranch, _tBranch } =
  (_tBranch @:) <$> L.indexed f False _fBranch

-- | Equivalent to 'tBranch' if applied to 'True', and 'fBranch' otherwise.
branch :: Bool
          -- ^ The branch to focus on.
       -> L.IndexedLens' Bool (If a) a
          -- ^ A lens over the chosen branch.
branch True  = tBranch
branch False = fBranch

-- | Indexed functorial map for if constructs.
imapIf :: (Bool -> a -> b) -> If a -> If b
imapIf f If { _fBranch, _tBranch } = f True _tBranch @: f False _fBranch

-- | Indexed fold-map for if constructs.
ifoldMapIf :: Monoid m => (Bool -> a -> m) -> If a -> m
ifoldMapIf f If { _fBranch, _tBranch } = f True _tBranch <> f False _fBranch

-- | Indexed traversal for if constructs
itraverseIf :: Applicative f => (Bool -> a -> f b) -> If a -> f (If b)
itraverseIf f If { _fBranch, _tBranch } =
  (@:) <$> f True _tBranch <*> f False _fBranch

-- | If constructs are indexed functors with the index as the branch taken.
instance L.FunctorWithIndex Bool If where imap = imapIf
-- | If constructs are indexed foldables with the index as the branch taken.
instance L.FoldableWithIndex Bool If where ifoldMap = ifoldMapIf
-- | If constructs are indexed traversables with the index as the branch taken.
instance L.TraversableWithIndex Bool If where itraverse = itraverseIf

