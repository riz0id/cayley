{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Cayley.ScopeSet
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'ScopeSet' type along with facilities for working
-- with sets of scopes.
--
-- @since 1.0.0
module Cayley.ScopeSet
  ( -- * ScopeSet
    ScopeSet (..)
    -- ** Basic Operations
  , empty
  , singleton
  , fromList
  , toList
  , fromSet
  , toSet
    -- ** Query
  , null
  , size
  , member
  , isSubsetOf
  , isProperSubsetOf
    -- ** Set Operations
  , union
  , intersection
  , difference
    -- ** Folds
  , foldr
  , foldr'
  , foldl
  , foldl'
  ) where

import Cayley.ScopeId (ScopeId (ScopeId))

import Data.Coerce (coerce)
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.IsList (IsList)
import GHC.IsList qualified as GHC

import Prelude hiding (foldl, foldr, null)

-- ScopeSet --------------------------------------------------------------------

-- | The 'ScopeSet' container is a set of 'ScopeId' that is used to lexical
-- scope of variable identifiers.
--
-- TODO: Optimization; replace the underlying representation 'Set' with a
-- 64-radix tree. This change is involved, but would be significantly more
-- memory efficient and performant.
--
-- @since 1.0.0
newtype ScopeSet = ScopeSet
  { getScopeSet :: Set ScopeId }
  deriving (Eq, Ord, Show)

-- | @since 1.0.0
instance Semigroup ScopeSet where
  (<>) = union
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid ScopeSet where
  mempty = empty
  {-# INLINE mempty #-}

-- | @since 1.0.0
instance IsList ScopeSet where
  type Item ScopeSet = ScopeId

  fromList = fromList
  {-# INLINE fromList #-}

  toList = toList
  {-# INLINE toList #-}

-- ScopeSet - Basic Operations -------------------------------------------------

-- | Construct an empty 'ScopeSet'.
--
-- @since 1.0.0
empty :: ScopeSet
empty = coerce Set.empty

-- | Construct a singleton 'ScopeSet'.
--
-- @since 1.0.0
singleton :: ScopeId -> ScopeSet
singleton = coerce Set.singleton

-- | Construct a 'ScopeSet' from a list of 'ScopeId's.
--
-- @since 1.0.0
fromList :: [ScopeId] -> ScopeSet
fromList = coerce Set.fromList

-- | Convert a 'ScopeSet' to a list of 'ScopeId's.
--
-- @since 1.0.0
toList :: ScopeSet -> [ScopeId]
toList = coerce Set.toList

-- | Convert a @'Set' 'ScopeId'@ to a 'ScopeSet'.
--
-- @since 1.0.0
fromSet :: Set ScopeId -> ScopeSet
fromSet = coerce

-- | Convert a 'ScopeSet' to a @'Set' 'ScopeId'@.
--
-- @since 1.0.0
toSet :: ScopeSet -> Set ScopeId
toSet = coerce

-- ScopeSet - Query ------------------------------------------------------------

-- | Is the given 'ScopeSet' empty?
--
-- @since 1.0.0
null :: ScopeSet -> Bool
null = coerce Set.null

-- | Obtain the number of elements in the given 'ScopeSet'.
--
-- @since 1.0.0
size :: ScopeSet -> Int
size = coerce Set.size

-- | Is the given 'ScopeId' a member of the 'ScopeSet'?
--
-- @since 1.0.0
member :: ScopeId -> ScopeSet -> Bool
member = coerce Set.member

-- | Is the given 'ScopeId' a subset of the 'ScopeSet'?
--
-- @since 1.0.0
isSubsetOf :: ScopeSet -> ScopeSet -> Bool
isSubsetOf = coerce Set.isSubsetOf

-- | Is the given 'ScopeSet' a *strict* subset of the 'ScopeSet'?
--
-- @since 1.0.0
isProperSubsetOf :: ScopeSet -> ScopeSet -> Bool
isProperSubsetOf = coerce Set.isProperSubsetOf

-- ScopeSet - Set Operations ---------------------------------------------------

-- | Take the union of the two given 'ScopeSet's.
--
-- @since 1.0.0
union :: ScopeSet -> ScopeSet -> ScopeSet
union = coerce Set.union

-- | Take the intersection of the two given 'ScopeSet's.
--
-- @since 1.0.0
intersection :: ScopeSet -> ScopeSet -> ScopeSet
intersection = coerce Set.intersection

-- | Take the difference of the two given 'ScopeSet's.
--
-- @since 1.0.0
difference :: ScopeSet -> ScopeSet -> ScopeSet
difference = coerce Set.difference

-- ScopeSet - Folds ------------------------------------------------------------

-- | Right-associative fold of a 'ScopeSet', lazy in the accumulator.
--
-- @since 1.0.0
foldr :: forall b. (ScopeId -> b -> b) -> b -> ScopeSet -> b
foldr = coerce @(_ -> b -> _) Set.foldr

-- | Right-associative fold of a 'ScopeSet', strict in the accumulator.
--
-- @since 1.0.0
foldr' :: forall b. (ScopeId -> b -> b) -> b -> ScopeSet -> b
foldr' = coerce @(_ -> b -> _) Set.foldr'

-- | Left-associative fold of a 'ScopeSet', lazy in the accumulator.
--
-- @since 1.0.0
foldl :: forall b. (b -> ScopeId -> b) -> b -> ScopeSet -> b
foldl = coerce @(_ -> b -> _) Set.foldl

-- | Left-associative fold of a 'ScopeSet', strict in the accumulator.
--
-- @since 1.0.0
foldl' :: forall b. (b -> ScopeId -> b) -> b -> ScopeSet -> b
foldl' = coerce @(_ -> b -> _) Set.foldl'