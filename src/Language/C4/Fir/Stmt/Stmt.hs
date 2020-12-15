{-# LANGUAGE DeriveTraversable
           , MultiParamTypeClasses
           , NamedFieldPuns
           , TemplateHaskell
           , TypeFamilies #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Stmt.Stmt
-- Description : Top-level statement definitions for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module contains 'Stmt', the main statement definition for FIR.
--------------------------------------------------------------------------------

module Language.C4.Fir.Stmt.Stmt
  ( -- * Statements
    Stmt (SMeta, Prim, Block, Lock, If, For)
    -- ** Recursion schemes
  , StmtF (SMetaF, PrimF, BlockF, LockF, IfF, ForF)
    -- ** Optics
  , stmtMeta
    -- ** Headers
  , BlockHeader  (ImpBlock, ExpBlock)
  , ForHeader    (_forInit, _forCond, _forUpdate)
  , IfHeader     (IfHeader)
  , LockHeader   (AtomicLock, SyncLock)
  , WhileHeader  (_whileKind, _whileCond)
  , WhileKind    (AWhile, ADoWhile)
    -- ** Optics
  , forInit
  , forCond
  , forUpdate
  , _IfHeader
  , whileKind
  , whileCond
  ) where

import qualified Control.Lens as L
import qualified Data.Functor.Foldable as F
import qualified Data.Functor.Foldable.TH as FTH
import Data.Bitraversable (bitraverse)

import Language.C4.Fir.If        (If)
import Language.C4.Fir.Expr.Expr (Expr, exprMeta)
import Language.C4.Fir.Stmt.Flow (StmtBlock, Flow, blockStmts)
import Language.C4.Fir.Stmt.Prim (PrimStmt, AsPrimStmt, _PrimStmt, primMeta)

-- | A flow block header for if statements.
newtype IfHeader e = IfHeader e
  deriving (Eq, Show, Functor, Foldable, Traversable)
L.makePrisms ''IfHeader

-- TODO(@MattWindsor91): replace the types in ForHeader with the right ones.

-- | A for loop header.
data ForHeader e
  = ForHeader
      { _forInit   :: Maybe e -- ^ The initialiser leg of the for loop.
      , _forCond   :: Maybe e -- ^ The condition leg of the for loop.
      , _forUpdate :: Maybe e -- ^ The update leg of the for loop.
      } deriving (Eq, Show, Functor, Foldable, Traversable)
L.makeLenses ''ForHeader 

-- | A kind of while loop.
data WhileKind
  = AWhile   -- ^ This is a while loop.
  | ADoWhile -- ^ This is a do-while loop.
    deriving (Eq, Show, Enum, Bounded)

-- | A while loop header.
data WhileHeader e
  = WhileHeader
      { _whileCond :: e         -- ^ Condition expression.
      , _whileKind :: WhileKind -- ^ Kind of while loop.
      } deriving (Eq, Show, Functor, Foldable, Traversable)
L.makeLenses ''WhileHeader

-- | A header for an implicit or explicit block.
-- 
-- While 'BlockHeader's carry an expression type, it is phantom.
data BlockHeader e
  = ExpBlock -- ^ Marks an explicit block.
  | ImpBlock -- ^ Marks an implicit block.
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Helper for changing the phantom metadata type on a block header.
castBlockHeaderMeta :: BlockHeader m1 -> BlockHeader m2
castBlockHeaderMeta ExpBlock = ExpBlock
castBlockHeaderMeta ImpBlock = ImpBlock

-- | A header for a transactional memory lock block.
--
-- While 'LockHeader's carry an expression type, it is phantom.
data LockHeader m
  = AtomicLock -- ^ Marks an atomic block.
  | SyncLock   -- ^ Marks a synchronised block.
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Helper for changing the phantom metadata type on a lock header.
castLockHeaderMeta :: LockHeader m1 -> LockHeader m2
castLockHeaderMeta AtomicLock = AtomicLock
castLockHeaderMeta SyncLock   = SyncLock

-- | A statement, parametrised over metadata.
data Stmt m
  = SMeta m (Stmt m)                                      -- ^ A metadata tag.
  | Prim  (PrimStmt (Expr m))                             -- ^ A primitive.
  | If    (Flow IfHeader    If         (Expr m) (Stmt m)) -- ^ An if statement.
  | For   (Flow ForHeader   L.Identity (Expr m) (Stmt m)) -- ^ A for loop.
  | While (Flow WhileHeader L.Identity (Expr m) (Stmt m)) -- ^ A while loop.
  | Block (Flow BlockHeader L.Identity (Expr m) (Stmt m)) -- ^ A direct block.
  | Lock  (Flow LockHeader  L.Identity (Expr m) (Stmt m)) -- ^ A lock block.
L.makePrisms ''Stmt
FTH.makeBaseFunctor ''Stmt

-- | We can turn 'PrimStmt' prisms on 'Stmt'.
instance AsPrimStmt (Stmt m) (Expr m) where _PrimStmt = _Prim

{-
flowMeta :: IsHeader h => L.Traversal (Flow h t m1 s) (Flow h t m2 s) m1 m2
flowMeta = header . headerMeta

flowStmts :: Traversable t => L.Traversal (Flow h t m s1) (Flow h t m s2) s1 s2
flowStmts = blocks . traverse . blockStmts . L.each
-}

-- | Traverses over the metadata inside a statement.
stmtMeta :: L.Traversal (Stmt m1) (Stmt m2) m1 m2
stmtMeta f = F.fold stmtMeta'
  where stmtMeta' (SMetaF m x) = SMeta <$> f m <*> x
        stmtMeta' (PrimF  p  ) = Prim  <$> primMeta f p
        stmtMeta' (IfF    x  ) = If    <$> flowStep f x
        stmtMeta' (ForF   x  ) = For   <$> flowStep f x
        stmtMeta' (WhileF x  ) = While <$> flowStep f x
        stmtMeta' (BlockF x  ) = Block <$> flowStep f x
        stmtMeta' (LockF  x  ) = Lock  <$> flowStep f x
        -- The type of this seems difficult to deduce if we make 'f' implicit.
        flowStep f = bitraverse (exprMeta f) id

