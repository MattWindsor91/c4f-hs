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
  ( AtomStmt
  , PrimStmt ( Atomic, Nop ) ) where

-- | Type of atomic statements.
data AtomStmt

-- | Type of 

-- | Type of primitive statements.
data PrimStmt
  = Atomic AtomStmt -- ^ An atomic statement.
  | Nop             -- ^ No-operation.
