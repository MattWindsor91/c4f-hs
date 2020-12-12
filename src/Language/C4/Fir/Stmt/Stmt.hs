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
    -- * Flow blocks
  , Flow (Flow, _header, _blocks)
    -- ** Headers
  , BlockHeader  (ImpBlock, ExpBlock)
  , ForHeader    (_forInit, _forCond, _forUpdate)
  , IfHeader     (IfHeader)
  , LockHeader   (AtomicLock, SyncLock)
  , WhileHeader  (_whileKind, _whileCond)
  , WhileKind    (AWhile, ADoWhile)
    -- ** Optics
  , header
  , blocks
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
  , StmtBlock   (StmtBlock, _blockStmts)
    -- ** Optics
  , blockStmts
  ) where

import qualified Control.Lens as L
import qualified Data.Functor.Foldable as F

import Language.C4.Fir.If        (If)
import Language.C4.Fir.Expr.Expr (Expr, exprMeta)
import Language.C4.Fir.Stmt.Prim (PrimStmt, AsPrimStmt, _PrimStmt, primMeta)

-- | Class of functionality common to all headers.
class IsHeader (h :: * -> *) where 
  -- | Traverses through all expressions in this header.
  --
  --   As expressions are the only place where headers contain metadata, such
  --   traversals can change the metadata.
  headerExprs :: L.Traversal (h m) (h m') (Expr m) (Expr m')

-- | Traverses through all metadata in this header.
--
--   As expressions are the only place where headers contain metadata, such
--   traversals can change the metadata.
headerMeta :: IsHeader h => L.Traversal (h m) (h m') m m'
headerMeta = headerExprs . exprMeta

-- | A flow block header for if statements.
newtype IfHeader m = IfHeader (Expr m)
L.makePrisms ''IfHeader

-- | If headers are headers where the header itself is the only expression.
instance IsHeader IfHeader where headerExprs = _IfHeader

-- TODO(@MattWindsor91): replace the types in ForHeader with the right ones.

-- | A for loop header.
data ForHeader m
  = ForHeader
      { _forInit   :: Maybe (Expr m) -- ^ The initialiser leg of the for loop.
      , _forCond   :: Maybe (Expr m) -- ^ The condition leg of the for loop.
      , _forUpdate :: Maybe (Expr m) -- ^ The update leg of the for loop.
      }
L.makeLenses ''ForHeader

-- | For headers are headers.
instance IsHeader ForHeader
  -- TODO(@MattWindsor91): fix up when we replace types
  where
    headerExprs f ForHeader { _forInit, _forCond, _forUpdate } =
      ( \ i c u -> ForHeader { _forInit=i, _forCond=c, _forUpdate = u} )
      <$> L.traverseOf L._Just f _forInit
      <*> L.traverseOf L._Just f _forCond
      <*> L.traverseOf L._Just f _forUpdate

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

-- | While headers are headers where the condition is the only expression.
instance IsHeader WhileHeader where headerExprs = whileCond

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

-- | A single block containing statements.
--
-- This datatype mainly exists to allow expansion into 
newtype StmtBlock s =
  StmtBlock { _blockStmts :: [s] -- ^ Block statements.
            } deriving (Eq, Show, Functor, Foldable, Traversable)
L.makeLenses ''StmtBlock

-- | Generalised structure of control-flow blocks.
--
-- Flow blocks contain two subcontainers: one is the flow header, and should
-- implement 'IsHeader'; the other is a flow block-set, and should implement
-- 'FoldableWithIndex' etc with some sort of index.
data Flow (h :: * -> *) (f :: * -> *) m s =
  Flow { _header :: h m
       , _blocks :: f (StmtBlock s)
       }
L.makeLenses ''Flow

instance Functor f => Functor (Flow h f m) where
  fmap f Flow { _header, _blocks } = Flow { _header, _blocks=bs }
    where bs = fmap (fmap f) _blocks

-- | A statement, parametrised over metadata.
data Stmt m
  = SMeta m (Stmt m)                               -- ^ A metadata tag.
  | Prim  (PrimStmt (Expr m))                      -- ^ A primitive statement.
  | If    (Flow IfHeader    If         m (Stmt m)) -- ^ An if statement.
  | For   (Flow ForHeader   L.Identity m (Stmt m)) -- ^ A for loop.
  | While (Flow WhileHeader L.Identity m (Stmt m)) -- ^ A while loop.
  | Block (Flow BlockHeader L.Identity m (Stmt m)) -- ^ A direct block.
  | Lock  (Flow LockHeader  L.Identity m (Stmt m)) -- ^ A lock block.
L.makePrisms ''Stmt

-- | We can turn 'PrimStmt' prisms on 'Stmt'.
instance AsPrimStmt (Stmt m) (Expr m) where _PrimStmt = _Prim

-- | Recursion-schemes base functor for 'Stmt'.
data StmtF m s
  = SMetaF m s                               -- ^ Non-recursive 'SMeta'.
  | PrimF  (PrimStmt (Expr m))               -- ^ Non-recursive 'Prim'.
  | IfF    (Flow IfHeader    If         m s) -- ^ Non-recursive 'If'.
  | ForF   (Flow ForHeader   L.Identity m s) -- ^ Non-recursive 'For'.
  | WhileF (Flow WhileHeader L.Identity m s) -- ^ Non-recursive 'While'.
  | BlockF (Flow BlockHeader L.Identity m s) -- ^ Non-recursive 'While'.
  | LockF  (Flow LockHeader  L.Identity m s) -- ^ Non-recursive 'While'.
    deriving Functor

type instance F.Base (Stmt m) = StmtF m
instance F.Recursive (Stmt m) where
  project (SMeta m x) = SMetaF m x
  project (Prim  p  ) = PrimF  p
  project (If    f  ) = IfF    f
  project (For   f  ) = ForF   f
  project (While f  ) = WhileF f
  project (Block f  ) = BlockF f
  project (Lock  f  ) = LockF  f
instance F.Corecursive (Stmt m) where
  embed (SMetaF m x) = SMeta m x
  embed (PrimF  p  ) = Prim  p
  embed (IfF    f  ) = If    f
  embed (ForF   f  ) = For   f
  embed (WhileF f  ) = While f
  embed (BlockF f  ) = Block f
  embed (LockF  f  ) = Lock  f

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
        flowStep f Flow { _header, _blocks } =
          (\h b -> Flow { _header=h, _blocks=b })
            <$> headerMeta f _header
            <*> traverse sequenceA _blocks
