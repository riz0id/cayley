{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Cayley.Identifier
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Identifier' type along with facilities for working
-- with identifiers.
--
-- @since 1.0.0
module Cayley.Identifier
  ( -- * Identifier
    Identifier (..)
    -- ** Basic Operations
  , newIdentifier
  ) where

import Cayley.ScopeSet (ScopeSet)
import Cayley.ScopeSet qualified as ScopeSet

import Data.ByteString (ByteString)

-- Identifier ------------------------------------------------------------------

-- | The type 'Identifier' is a symbolic identifier along with the scope of the
-- identifier.
--
-- @since 1.0.0
data Identifier = Identifier
  { identifier_symbol :: {-# UNPACK #-} !ByteString
    -- ^ The lexical name of the identifier.
  , identifier_scopes :: ScopeSet
    -- ^ The de Bruijn index uniquely determining the identifier.
  }
  deriving (Show)

-- Identifier - Basic Operations -----------------------------------------------

-- | Construct a new 'Identifier' from the given 'ByteString' symbol.
--
-- @since 1.0.0
newIdentifier :: ByteString -> Identifier
newIdentifier s = Identifier s ScopeSet.empty
{-# INLINE CONLIKE newIdentifier #-}