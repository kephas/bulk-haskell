{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Eval.Types where

import Control.Lens (makeLenses)
import Data.Map.Strict qualified as M
import Data.Word (Word8)

import Data.BULK.Types (BULK (..), NamespaceDefinition (..), Package (..))

data Scope = Scope
    { _associatedNamespaces :: M.Map Int NamespaceDefinition
    , _definitions :: M.Map BULK BULK
    , _knownNamespaces :: [NamespaceDefinition]
    , _knownPackages :: [Package]
    , _definingNamespace :: Maybe IncompleteNamespace
    }

data Definition

data IncompleteNamespace = IncompleteNamespace
    { _namespaceDefinition :: NamespaceDefinition
    , _nextName :: Word8
    , _pendingDefinitions :: [(Word8, BULK)]
    }

makeLenses ''Scope
makeLenses ''IncompleteNamespace
