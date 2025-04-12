{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Cayley.ScopeId
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'ScopeId' type.
--
-- @since 1.0.0
module Cayley.ScopeId
  ( -- * ScopeId
    ScopeId (..)
    -- ** Basic Operations
  , newScopeId
  ) where

import Control.Monad.IO.Class (MonadIO (..))

import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Hashable (Hashable)

import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- | Internal mutable state tracking the next unused 'ScopeId'.
--
-- @since 1.0.0
scopeIdSource :: IORef ScopeId
scopeIdSource = unsafePerformIO (IORef.newIORef (ScopeId 0))
{-# NOINLINE scopeIdSource #-}

-- ScopeId ---------------------------------------------------------------------

-- | The type 'ScopeId' is a unique identifier that is used to track the lexical
-- scope of variables.
--
-- @since 1.0.0
newtype ScopeId = ScopeId
  { getScopeId :: Word }
  deriving newtype (Enum, Eq, Hashable, Ord, Show)

-- ScopeId - Basic Operations --------------------------------------------------

-- | Generate a new and unused 'ScopeId'.
--
-- @since 1.0.0
newScopeId :: MonadIO m => m ScopeId
newScopeId = liftIO (IORef.atomicModifyIORef' scopeIdSource next)
  where
    next :: ScopeId -> (ScopeId, ScopeId)
    next x = let x' = succ x in (x', x')
{-# SPECIALISE INLINE newScopeId :: IO ScopeId #-}