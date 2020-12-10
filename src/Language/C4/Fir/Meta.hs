{-# LANGUAGE MultiParamTypeClasses, KindSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Meta
-- Description : Basic metadata support for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module contains helpers for dealing with the metadata embedded in FIR
-- statements and expressions.  It doesn't, itself, define a metadata type.
--------------------------------------------------------------------------------

module Language.C4.Fir.Meta
  ( -- * Class of things that produce metadata
    Meta (exMeta, metaString)
  ) where

-- | Class of metadata types.
class Meta m where
  -- | Metadata that represents an item that existed before the fuzzer ran.
  exMeta :: m
  -- | Converts metadata to a string.
  --
  --   This is partial, as some metadata might not need a string
  --   representation.
  metaString :: m -> Maybe String
