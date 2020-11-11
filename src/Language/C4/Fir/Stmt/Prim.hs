--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Stmt.Atomic
-- Description : C4 Fuzzable Internal Representation: Atomic statements
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- Fir constants, convenience constructors, and type coersion.
--
-- The general rule for Boolean coersion is that 0 is false and everything else
-- is true; when converting integers to booleans, we further specificially map
-- true to 1.
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
