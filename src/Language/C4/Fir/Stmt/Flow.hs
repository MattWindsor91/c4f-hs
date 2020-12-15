{-# LANGUAGE DeriveTraversable
           , MultiParamTypeClasses
           , NamedFieldPuns
           , TemplateHaskell
           , TypeFamilies #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Stmt.Flow
-- Description : Generic control-flow constructs for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module contains 'Flow', FIR's main abstraction over control flow; it
-- also contains 'StmtBlock' (blocks containing statements).
--------------------------------------------------------------------------------

module Language.C4.Fir.Stmt.Flow 
  ( StmtBlock (StmtBlock, _blockStmts)
  , Flow      (Flow, _header, _blocks)
  , (@<)
    -- * Optics
  , blockStmts
  , header
  , blocks
  ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Bifunctor.Biff (Biff (..))
import Data.Functor.Compose (Compose (..))
import qualified Control.Lens as L

-- | A single block containing statements.
--
-- This datatype mainly exists to allow expansion with parameters later on.
newtype StmtBlock s =
  StmtBlock { _blockStmts :: [s] -- ^ Block statements.
            } deriving (Eq, Show, Functor, Foldable, Traversable)
L.makeLenses ''StmtBlock

-- | Generalised structure of control-flow blocks.
--
-- Flow blocks contain two subcontainers: the flow header 'h', which can contain
-- expressions; and the block set 'b', which can contain statements indirectly
-- through zero or more 'StmtBlock's.
--
-- Generally, 'h' should implement a traversal over expressions, and
-- the block-set should implement an indexed traversal over blocks. 
data Flow (h :: * -> *) (b :: * -> *) e s =
  Flow { _header :: h e
       , _blocks :: b (StmtBlock s)
       }
L.makeLenses ''Flow

-- | Reinterprets a flow block as a tuple 'Biff'ed onto a header and a composed
--   functor made from the block set and 'StmtBlock'.
flowBf :: L.Iso (Flow h  b  e  s )
                (Flow h' b' e' s')
                (Biff (,) h  (Compose b  StmtBlock) e  s )
                (Biff (,) h' (Compose b' StmtBlock) e' s')
flowBf = L.iso biff ffib
  where biff Flow { _header, _blocks } = Biff (_header, Compose _blocks)
        ffib (Biff (_header, Compose _blocks)) = Flow { _header, _blocks }

-- | Infix shorthand for constructing flow blocks.
(@<) :: h e -> f (StmtBlock s) -> Flow h f e s
_header @< _blocks = Flow { _header, _blocks }

{-
 - Bi-traversing
 -}

-- | Flow blocks form bifunctors over their expressions and statements.
instance (Functor h, Functor b) => L.Bifunctor (Flow h b) where
  bimap f g = L.over flowBf (L.bimap f g)

-- | Flow blocks form bifolds over their expressions and statements.
instance (Foldable h, Foldable b) => Bifoldable (Flow h b) where
  bifoldMap f g = bifoldMap f g . L.view flowBf

-- | Flow blocks form bitraversables over their expressions and statements.
instance (Traversable h, Traversable b) => Bitraversable (Flow h b) where
  bitraverse f g = flowBf (bitraverse f g)

{-
 - Traversing over statements
 -}

-- | Flow blocks form functors over their statements.
instance Functor b => Functor (Flow h b e) where
  fmap f Flow { _header, _blocks } = _header @< fmap (fmap f) _blocks

-- | Flow blocks form foldables over their statements.
instance Foldable b => Foldable (Flow h b e) where
  foldMap f Flow { _blocks } = foldMap (foldMap f) _blocks

-- | Flow blocks form traversables over their statements.
instance Traversable b => Traversable (Flow h b e) where
  traverse f Flow { _header, _blocks } =
    (_header @<) <$> traverse (traverse f) _blocks
