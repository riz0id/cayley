{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Cayley.Core
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Cayley.Core
  ( -- * Literal
    Literal (..)
    -- * Expr
  , Expr (..)
  ) where

import Data.Kind (Type)

-- Literal ---------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Literal :: Type where
  LitRat :: Rational -> Literal
  deriving (Show)

-- Expr ------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Expr :: Type where
  ExprLit :: Literal -> Expr
  deriving (Show)
