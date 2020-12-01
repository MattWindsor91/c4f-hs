{-# LANGUAGE TemplateHaskell #-}
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
  , -- * Optics
    AsPrimStmt
  , _PrimStmt
  , _Nop
  ) where

import qualified Control.Lens as L
-- | Type of primitive statements.
data PrimStmt
  = Nop             -- ^ No-operation.
L.makeClassyPrisms ''PrimStmt
