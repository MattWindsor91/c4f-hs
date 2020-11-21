--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Expr.Op
-- Description : Operator definitions for FIR
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--
-- This module defines the various operators available both to FIR expressions
-- and atomic read-modify-writes.
--
-- With all of the operator groupings, the order is arbitrary and only
-- meaningful for situations such as building operator maps.
--------------------------------------------------------------------------------

module Language.C4.Fir.Expr.Op
  ( -- * Binary operators
    ABop ((:+), (:-))
  , BBop ((:&), (:|), (:^))
  , LBop ((:&&), (:||))
  , RBop ((:<), (:<=), (:>), (:>=), (:==), (:!=))
  , Bop (Arith, Bitwise, Logical, Rel)
    -- * Unary operators
  , Uop (Comp, Not)
    -- * Predicates
  , relIncl -- :: RBop -> RBop -> Bool
    -- * Semantics
  , semABop -- :: Num a => ABop -> a -> a -> a
  , semBBop -- :: Bits a => BBop -> a -> a -> a
  , semLBop -- :: Monad m => LBop -> m Bool -> m Bool -> m Bool
  , semRBop -- :: (Eq a, Ord a) -> RBop -> a -> a -> Bool
  ) where

import Data.Bits (Bits, (.&.), (.|.), xor)

{-
 - Binary operators
 -}

-- | Enumeration of all binary operators.
--
-- We group operators into four semantic groupings: arithmetic, bitwise,
-- logical, and relational.
data Bop
  = Arith   ABop -- ^ An arithmetic operator.
  | Bitwise BBop -- ^ A bitwise operator.
  | Logical LBop -- ^ A logical operator.
  | Rel     RBop -- ^ A relational operator.
    deriving (Eq, Show)

-- | Arithmetic binary operators.
--
-- Arithmetic operators take integers and produce integers.
data ABop
  = (:+) -- ^ Addition.
  | (:-) -- ^ Subtraction.
    deriving (Eq, Ord, Show, Bounded, Enum)

-- | Semantics of arithmetic binary operators, defined over any integer type.
semABop :: Num a => ABop -> a -> a -> a
semABop (:+) = (+)
semABop (:-) = (-)

-- | Bitwise binary operators.
--
-- Bitwise operators also take integers and produce integers, but operate on the
-- bits of their operands.
data BBop
  = (:&) -- ^ Bitwise AND.
  | (:|) -- ^ Bitwise OR.
  | (:^) -- ^ Bitwise XOR.
    deriving (Eq, Ord, Show, Bounded, Enum)

-- If we introduce shifts, they will form a separate class, as they combine
-- a bit operand and an arithmetic operand.

-- | Semantics of bitwise binary operators, defined over any type with bits.
semBBop :: Bits a => BBop -> a -> a -> a
semBBop (:&) = (.&.)
semBBop (:|) = (.|.)
semBBop (:^) = xor

-- | Logical binary operators.
--
-- Logical operators take Booleans and produce Booleans, and have
-- short-circuiting semantics.
data LBop
  = (:&&) -- ^ Conjunction.
  | (:||) -- ^ Disjunction.
    deriving (Eq, Ord, Show, Bounded, Enum)

-- | Semantics of logical binary operators, defined over a monad to allow for
-- the capture of short-circuit evaluation.
semLBop :: Monad m => LBop -> m Bool -> m Bool -> m Bool
--                           LHS TRUE            LHS FALSE
semLBop (:&&) = shortCircuit id                  (const (pure False))
semLBop (:||) = shortCircuit (const (pure True)) id

-- Captures short-circuiting semantics over an operator.
shortCircuit :: Monad m
             => (m Bool -> m Bool) -- ^ Computation when LHS is true.
             -> (m Bool -> m Bool) -- ^ Computation when LHS is false.
             -> m Bool             -- ^ Computation to generate LHS.
             -> m Bool             -- ^ Computation to generate RHS.
             -> m Bool             -- ^ Computation of final value.
shortCircuit tf ff ml mr =
  do l <- ml
     (if l then tf else ff) mr


-- | Relational binary operators.
--
-- Relational operators take two operands of the same type, and produce Booleans.
data RBop
  = (:<) -- ^ Less-than.
  | (:<=) -- ^ Less-than-or-equal.
  | (:>) -- ^ Greater-than.
  | (:>=) -- ^ Greater-than-or-equal.
  | (:==) -- ^ Equal.
  | (:!=) -- ^ Not-equal.
    deriving (Eq, Ord, Show, Bounded, Enum)

-- | Semantics of relational binary operators, defined over anything with an
--   equality and ordering.
semRBop :: (Eq a, Ord a) => RBop -> a -> a -> Bool
semRBop (:<) = (<)
semRBop (:<=) = (<=)
semRBop (:>) = (>)
semRBop (:>=) = (>=)
semRBop (:==) = (==)
semRBop (:!=) = (/=)

-- | True if, and only if, the LHS relation includes the RHS relation. -}
relIncl :: RBop -> RBop -> Bool
-- Reflexive cases first:
relIncl (:< ) (:< ) = True
relIncl (:<=) (:<=) = True
relIncl (:> ) (:> ) = True
relIncl (:>=) (:>=) = True
relIncl (:==) (:==) = True
relIncl (:!=) (:!=) = True
-- These operators are composites of other relations:
relIncl (:<=) (:< ) = True
relIncl (:<=) (:==) = True
relIncl (:>=) (:> ) = True
relIncl (:>=) (:==) = True
-- Other cases are invalid:
relIncl _     _     = False

{-
 - Unary operators
 -}


-- | Enumeration of all unary operators.
--
-- There aren't enough unary operators to benefit from grouping, so we just have
-- one linear enumeration.
data Uop
  = Comp -- ^ Bitwise complement.
  | Not -- ^ Logical negation.
    deriving (Eq, Ord, Show, Bounded, Enum)

-- We don't give the semantics of Uop here, as there are only two operators and
-- they take different types.
