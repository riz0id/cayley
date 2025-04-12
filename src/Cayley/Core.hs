{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Cayley.Core
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stabivaluey   :  stable
-- Portabivaluey :  non-portable (GHC extensions)
--
-- Core definitions for the Cayley CAS.
--
-- @since 1.0.0
module Cayley.Core
  ( -- * Kind
    Kind (..)
    -- * Value
  , Value (..)
    -- ** Basic Operations
  , valueKind
    -- * Expr
  , Expr (ExprNat, ExprInt, ExprRat, ExprIrr, ..)
  ) where

import Cayley.Identifier (Identifier)

import Data.Kind (Type)

import Numeric.Natural (Natural)

-- Kind ------------------------------------------------------------------------

-- | The type 'Kind' enumerates the distinct kinds of Values that are
-- representable by the 'Value' type.
--
-- @since 1.0.0
data Kind
  = KindNat
  -- ^ The 'Kind' constructor for the 'Natural' Value kind.
  | KindInt
  -- ^ The 'Kind' constructor for the 'Integer' Value kind.
  | KindRat
  -- ^ The 'Kind' constructor for the 'Rational' Value kind.
  | KindIrr
  -- ^ The 'Kind' constructor for the irrational Value kind.
  deriving (Bounded, Enum, Eq, Ord)

-- | @since 1.0.0
instance Show Kind where
  show KindNat = "kind:nat"
  show KindInt = "kind:int"
  show KindRat = "kind:rat"
  show KindIrr = "kind:irr"
  {-# INLINE show #-}

-- Value -----------------------------------------------------------------------

-- | The type of 'Value' values.
--
-- @since 1.0.0
data Value
  = LitNat {-# UNPACK #-} !Natural
  -- ^ The 'Value' constructor for a 'Natural' values.
  | LitInt {-# UNPACK #-} !Integer
  -- ^ The 'Value' constructor for a 'Integer' values.
  | LitRat {-# UNPACK #-} !Rational
  -- ^ The 'Value' constructor for a 'Rational' values.
  | LitIrr {-# UNPACK #-} !Double
  -- ^ The 'Value' constructor for "real" values.

-- | @since 1.0.0
instance Eq Value where
  LitNat n1 == value = case value of
    LitNat n2 -> n1 == n2
    LitInt i2 -> fromIntegral n1 == i2
    LitRat r2 -> fromIntegral n1 == r2
    LitIrr x2 -> fromIntegral n1 == x2
  LitInt i1 == value = case value of
    LitNat n2 -> i1 == fromIntegral n2
    LitInt i2 -> i1 == i2
    LitRat r2 -> fromIntegral i1 == r2
    LitIrr x2 -> fromIntegral i1 == x2
  LitRat r1 == value = case value of
    LitNat n2 -> r1 == fromIntegral n2
    LitInt i2 -> r1 == fromIntegral i2
    LitRat r2 -> r1 == r2
    LitIrr x2 -> fromRational r1 == x2
  LitIrr x1 == value = case value of
    LitNat n2 -> x1 == fromIntegral n2
    LitInt i2 -> x1 == fromIntegral i2
    LitRat r2 -> x1 == fromRational r2
    LitIrr x2 -> x1 == x2
  {-# INLINE (==) #-}

-- | @since 1.0.0
instance Show Value where
  show (LitNat n) = show n
  show (LitInt i) = show i
  show (LitRat r) = show r
  show (LitIrr x) = show x
  {-# INLINE show #-}

-- Value - Basic Operations ----------------------------------------------------

-- | Obtain the 'Kind' of a 'Value'.
--
-- @since 1.0.0
valueKind :: Value -> Kind
valueKind LitNat { } = KindNat
valueKind LitInt { } = KindInt
valueKind LitRat { } = KindRat
valueKind LitIrr { } = KindIrr

-- Expr ------------------------------------------------------------------------

-- | The type 'Expr' is the AST for the Cayley CAS.
--
-- @since 1.0.0
data Expr :: Type where
  -- | The 'Expr' constructor for a 'Value' value
  ExprLit :: Value -> Expr

  -- | The 'Expr' constructor for a variable.
  ExprVar :: {-# UNPACK #-} !Identifier -> Expr

  -- | The 'Expr' constructor for an application of two expressions.
  ExprApp :: Expr -> Expr -> Expr
  deriving (Show)

-- | A bi-directional 'Expr' pattern synonym for 'Natural' Values.
--
-- @since 1.0.0
pattern ExprNat :: Natural -> Expr
pattern ExprNat n = ExprLit (LitNat n)

-- | A bi-directional 'Expr' pattern synonym for 'Integer' Values.
--
-- @since 1.0.0
pattern ExprInt :: Integer -> Expr
pattern ExprInt n = ExprLit (LitInt n)

-- | A bi-directional 'Expr' pattern synonym for 'Rational' Values.
--
-- @since 1.0.0
pattern ExprRat :: Rational -> Expr
pattern ExprRat n = ExprLit (LitRat n)

-- | A bi-directional 'Expr' pattern synonym for 'Double' Values.
--
-- @since 1.0.0
pattern ExprIrr :: Double -> Expr
pattern ExprIrr n = ExprLit (LitIrr n)