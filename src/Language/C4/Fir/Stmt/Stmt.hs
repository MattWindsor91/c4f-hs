{-# LANGUAGE DeriveTraversable
           , KindSignatures
           , MultiParamTypeClasses
           , NamedFieldPuns
           , TemplateHaskell #-}
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
    Stmt (Prim, Block, If, For)
    -- * Flow blocks
  , Flow (Flow, _header, _blocks)
    -- ** Headers
  , BlockHeader (ImpBlock, ExpBlock)
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
    -- ** Typeclass
  , IsHeader    (headerExprs)
    -- ** Optics
    -- * Components
  , StmtBlock   (StmtBlock, _blockMeta, _blockStmts)
  , IfBlock     (IfBlock, _tBranch, _fBranch)
    -- ** Optics
  , blockMeta
  , blockStmts
  , tBranch
  , fBranch
  ) where

import qualified Control.Lens as L
import Language.C4.Fir.Expr.Expr (Expr)
import Language.C4.Fir.Stmt.Prim (PrimStmt, AsPrimStmt, _PrimStmt)

-- | A flow block header for if statements.
newtype IfHeader m = IfHeader (Expr m)
L.makePrisms ''IfHeader

-- TODO(@MattWindsor91): replace the types in ForHeader with the right ones.

-- | Class of functionality common to all headers.
class IsHeader (h :: * -> *) where 
  -- | Traverses through all expressions in this header.
  --
  --   As expressions are the only place where headers contain metadata, such
  --   traversals can change the metadata.
  headerExprs :: L.Traversal (h m) (h m') (Expr m) (Expr m')

-- | A for loop header.
data ForHeader m
  = ForHeader
      { _forInit   :: Maybe (Expr m) -- ^ The initialiser leg of the for loop.
      , _forCond   :: Maybe (Expr m) -- ^ The condition leg of the for loop.
      , _forUpdate :: Maybe (Expr m) -- ^ The update leg of the for loop.
      }
L.makeLenses ''ForHeader

-- | A kind of while loop.
data WhileKind
  = AWhile   -- ^ This is a while loop.
  | ADoWhile -- ^ This is a do-while loop.

-- | A while loop header.
data WhileHeader m
  = WhileHeader
      { _whileCond :: Expr m    -- ^ Condition expression.
      , _whileKind :: WhileKind -- ^ Kind of while loop.
      }
L.makeLenses ''WhileHeader

-- | A header for an implicit or explicit block.
data BlockHeader m
  = ExpBlock -- ^ Marks an explicit block.
  | ImpBlock -- ^ Marks an implicit block.

-- | Helper for changing the phantom metadata type on a block header.
castBlockHeaderMeta :: BlockHeader m1 -> BlockHeader m2
castBlockHeaderMeta ExpBlock = ExpBlock
castBlockHeaderMeta ImpBlock = ImpBlock

-- | Block headers are headers with no expressions.
instance IsHeader BlockHeader where
  headerExprs = const (pure . castBlockHeaderMeta)

-- | A header for a transactional memory lock block.
data LockHeader m
  = AtomicLock -- ^ Marks an atomic block.
  | SyncLock   -- ^ Marks a synchronised block.

-- | Helper for changing the phantom metadata type on a lock header.
castLockHeaderMeta :: LockHeader m1 -> LockHeader m2
castLockHeaderMeta AtomicLock = AtomicLock
castLockHeaderMeta SyncLock   = SyncLock

-- | Lock headers are headers with no expressions.
instance IsHeader LockHeader where
  headerExprs = const (pure . castLockHeaderMeta)

-- | A single block containing statements and metadata.
data StmtBlock m s =
  StmtBlock { _blockMeta  :: m   -- ^ Block-level metadata.
            , _blockStmts :: [s] -- ^ Block statements.
            } deriving (Eq, Show)
L.makeLenses ''StmtBlock

-- | 'StmtBlock's are bifunctors where 'first' operates over the immediate
--   block metadata, and 'second' operates over block statements.
instance L.Bifunctor StmtBlock where
  bimap f g StmtBlock { _blockMeta, _blockStmts } =
    StmtBlock { _blockMeta = f _blockMeta, _blockStmts = g <$> _blockStmts }

-- | A pair of blocks representing the branches of an if statement.
data IfBlock b
  = IfBlock
      { _tBranch :: b
      , _fBranch :: b
      } deriving (Functor, Foldable, Traversable)
L.makeLenses ''IfBlock

instance L.FunctorWithIndex Bool IfBlock where
  imap f IfBlock { _tBranch, _fBranch } =
    IfBlock { _tBranch = f True _tBranch, _fBranch = f False _fBranch }

-- | Generalised structure of control-flow blocks.
--
-- Flow blocks contain two subcontainers: one is the flow header, and should
-- implement 'IsHeader'; the other is a flow block-set, and should implement
-- 'FoldableWithIndex' etc with some sort of index.
data Flow (h :: * -> *) (f :: * -> *) m s =
  Flow { _header :: h m
       , _blocks :: f (StmtBlock m s)
       }

-- | A statement, parametrised over metadata.
data Stmt m
  = Prim PrimStmt
    -- ^ A primitive statement.
  | If  (Flow IfHeader IfBlock m (Stmt m))
    -- ^ An if statement.
  | For (Flow ForHeader L.Identity m (Stmt m))
    -- ^ A for loop.
  | While (Flow WhileHeader L.Identity m (Stmt m))
    -- ^ A while loop.
  | Block (Flow BlockHeader L.Identity m (Stmt m))
    -- ^ A direct block.
  | Lock (Flow LockHeader L.Identity m (Stmt m))
    -- ^ A lock block.
L.makePrisms ''Stmt

-- | We can turn 'PrimStmt' prisms on 'Stmt'.
instance AsPrimStmt (Stmt m) where _PrimStmt = _Prim
