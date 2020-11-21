{-# LANGUAGE DeriveFunctor, TemplateHaskell, TypeFamilies #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.C4.Fir.Expr.Expr
-- Description : C4 Fuzzable Internal Representation: Expressions
-- Copyright   : (c) Matt Windsor, 2018, 2019, 2020
-- License     : MIT
-- Maintainer  : mattwindsor91@gmail.com
-- Stability   : experimental
--------------------------------------------------------------------------------

module Language.C4.Fir.Expr.Expr
  ( Expr
      ( Meta
      , ALoad
      , Prim
      , Bin
      , Un
      )
  , PExpr (Con, Addr)
    -- * Recursion schemes
  , ExprF
      ( MetaF
      , ALoadF
      , PrimF
      , BinF
      , UnF
      )
    -- * Binary operator shorthand
  , arith
  , (@+) -- :: Expr m -> Expr m -> Expr m
  , (@-) -- :: Expr m -> Expr m -> Expr m
  , rel
  , (@<) -- :: Expr m -> Expr m -> Expr m
  , (@<=) -- :: Expr m -> Expr m -> Expr m
  , (@>) -- :: Expr m -> Expr m -> Expr m
  , (@>=) -- :: Expr m -> Expr m -> Expr m
  , (@==) -- :: Expr m -> Expr m -> Expr m
  , (@!=) -- :: Expr m -> Expr m -> Expr m
  , bitwise
  , (@&) -- :: Expr m -> Expr m -> Expr m
  , (@^) -- :: Expr m -> Expr m -> Expr m
  , (@|) -- :: Expr m -> Expr m -> Expr m
  , logical
  , (@&&) -- :: Expr m -> Expr m -> Expr m
  , (@||) -- :: Expr m -> Expr m -> Expr m
  ) where

import Control.Lens (makePrisms)
import qualified Data.Functor.Foldable as F
import Language.C4.Fir.Atomic.Action (Load)
import Language.C4.Fir.Const (AsConst, Const, _Const)
import Language.C4.Fir.Lvalue (Address)
import Language.C4.Fir.Expr.Op
  (ABop (..), BBop (..), LBop (..), RBop (..), Bop (..), Uop (..) )

-- | Primitive expressions.
--
--   These are expressions that depend neither on recursive expressions nor
--   on metadata; we factor them out mainly to simplify things like recursion
--   schemes.
data PExpr
  = Con  Const     -- ^ Constant expression.
  | Addr Address   -- ^ Address expression.
    deriving (Eq, Show)

-- There doesn't seem to be a good reason for PExprs to be classy.
makePrisms ''PExpr

instance AsConst PExpr where _Const = _Con

-- | Top-level expressions.
data Expr m
  = Meta  m (Expr m)            -- ^ Wraps an expression in metadata.
  | Prim  PExpr                 -- ^ Primitive expression.
  | ALoad (Load (Expr m))       -- ^ Atomic load.
  | Bin   Bop (Expr m) (Expr m) -- ^ Binary operation.
  | Un    Uop (Expr m)          -- ^ Unary operation.
    deriving (Eq, Show)

{- We don't make classy prisms for Exprs, because the introduction of the
   metadata parameter causes the resulting class to be very unwieldy.  This may
   change in future if we really need it.  -}
makePrisms ''Expr

instance AsConst (Expr m) where _Const = _Prim . _Con

{-
 - Binary operator shorthand
 -}

-- | Constructs an arithmetic binary expression.
arith :: ABop -> Expr m -> Expr m -> Expr m
arith = Bin . Arith

-- | Constructs an addition.
(@+) :: Expr m -> Expr m -> Expr m
(@+) = arith (:+)
infixl 7 @+

-- | Constructs a subtraction.
(@-) :: Expr m -> Expr m -> Expr m
(@-) = arith (:-)
infixl 7 @-

-- If we add multiplication, it goes here.

-- | Constructs a relational binary expression.
rel :: RBop -> Expr m -> Expr m -> Expr m
rel = Bin . Rel

-- | Constructs a less-than.
(@<) :: Expr m -> Expr m -> Expr m
(@<) = rel (:<)
infixl 6 @<

-- | Constructs a less-than-or-equal.
(@<=) :: Expr m -> Expr m -> Expr m
(@<=) = rel (:<=)
infixl 6 @<=

-- | Constructs a greater-than.
(@>) :: Expr m -> Expr m -> Expr m
(@>) = rel (:>)
infixl 6 @>

-- | Constructs a greater-than-or-equal.
(@>=) :: Expr m -> Expr m -> Expr m
(@>=) = rel (:>=)
infixl 6 @>=

-- | Constructs an equality.
(@==) :: Expr m -> Expr m -> Expr m
(@==) = rel (:==)
infixl 5 @==

-- | Constructs a non-equality.
(@!=) :: Expr m -> Expr m -> Expr m
(@!=) = rel (:!=)
infixl 5 @!=

-- | Constructs a bitwise binary expression.
bitwise :: BBop -> Expr m -> Expr m -> Expr m
bitwise = Bin . Bitwise

-- | Constructs a bitwise AND.
(@&) :: Expr m -> Expr m -> Expr m
(@&) = bitwise (:&)
infixl 4 @&

-- | Constructs a bitwise XOR.
(@^) :: Expr m -> Expr m -> Expr m
(@^) = bitwise (:^)
infixl 3 @^

-- | Constructs a bitwise OR.
(@|) :: Expr m -> Expr m -> Expr m
(@|) = bitwise (:|)
infixl 2 @|

-- | Constructs a logical binary expression.
logical :: LBop -> Expr m -> Expr m -> Expr m
logical = Bin . Logical

-- | Constructs a logical AND.
(@&&) :: Expr m -> Expr m -> Expr m
(@&&) = Bin (Logical (:&&))
infixl 1 @&&

-- | Constructs a logical OR.
(@||) :: Expr m -> Expr m -> Expr m
(@||) = Bin (Logical (:||))
infixl 0 @||

{-
 - Recursion schemes
 -}

-- Base functor for 'Expr'.
--
-- The 'Expr' type with all instances of recursion into 'Expr' replaced
-- with a free type parameter.
data ExprF m b
  = MetaF  m b      -- ^ Non-recursive form of 'Meta'.
  | PrimF  PExpr    -- ^ Non-recursive form of 'Prim'.
  | ALoadF (Load b) -- ^ Non-recursive form of 'ALoad'.
  | BinF   Bop b b  -- ^ Non-recursive form of 'Bin'.
  | UnF    Uop b    -- ^ Non-recursive form of 'Un'.
    deriving Functor

-- The usual `recursion-schemes` boilerplate follows:
type instance F.Base (Expr m) = ExprF m
instance F.Recursive (Expr m) where
  project (Meta  m x  ) = MetaF m x
  project (Prim  x    ) = PrimF x
  project (ALoad x    ) = ALoadF x
  project (Bin   o l r) = BinF  o l r
  project (Un    o x  ) = UnF   o x
instance F.Corecursive (Expr m) where
  embed (MetaF  m x  ) = Meta  m x
  embed (PrimF  x    ) = Prim  x
  embed (ALoadF x    ) = ALoad x
  embed (BinF   o l r) = Bin   o l r
  embed (UnF    o x  ) = Un    o x
