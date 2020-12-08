{-# LANGUAGE DeriveTraversable, NamedFieldPuns, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Stmt.Prim
-- Description : Primitive statement definitions for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------

module Language.C4.Fir.Stmt.Prim
  ( PrimStmt ( Nop )
    -- * Optics
  , AsPrimStmt
  , _PrimStmt
  , _Nop
    -- * Assignments
  , AssignSrc (AssignDec, AssignInc, AssignExpr)
  , Assign (Assign, _dst, _src)
  , (@=)
  ) where

import qualified Control.Lens as L
import qualified Language.C4.Fir.Lvalue    as V
import qualified Language.C4.Fir.Expr.Expr as X

-- | Sources of assignment statements.
--
--   These don't yet capture every possible C assignment, but may expand as and
--   when needed.
--
--   The type parameter is that of any expression.
data AssignSrc e = AssignDec     -- ^ Assign by postfix decrement.
                 | AssignInc     -- ^ Assign by postfix increment.
                 | AssignExpr e  -- ^ Assign an expression.
                   deriving ( Eq
                            , Functor     -- ^ Map over any expressions.
                            , Foldable    -- ^ Fold over any expressions.
                            , Traversable -- ^ Traverse over any expressions.
                            )

data Assign e =
  Assign { _dst :: V.Lvalue
         , _src :: AssignSrc e
         } deriving ( Eq
                    , Functor -- ^ Map over any expressions.
                    , Foldable -- ^ Fold over any expressions.
                    , Traversable -- ^ Traverse over any expressions.
                    )

-- | Shorthand for an assignment of an expression to an lvalue-like.
(@=) :: V.Lvalue
        -- ^ The destination of the assignment.
     -> e
        -- ^ The expression to assign.
     -> Assign e
        -- ^ The assignment statement.
_dst @= e = Assign { _dst, _src= AssignExpr e }

-- | Type of primitive statements.
data PrimStmt
  = Nop             -- ^ No-operation.
L.makeClassyPrisms ''PrimStmt
