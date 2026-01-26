{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Eval.Types where

import Control.Lens (Prism', makeLenses, prism')
import Data.Map.Strict qualified as M
import Data.Word (Word8)

import Data.BULK.Types (BULK (..), CheckDigest, Name, NamespaceDefinition (..), Package (..))
import Data.BULK.Types qualified as Core (LazyFunction (..))

data Scope = Scope
    { _associatedNamespaces :: M.Map Int NamespaceDefinition
    , _definitions :: M.Map Name Value
    , _knownNamespaces :: [NamespaceDefinition]
    , _knownPackages :: [Package]
    , _definingNamespace :: Maybe IncompleteNamespace
    }

data Value = Expression BULK | Digest CheckDigest | LazyFunction Core.LazyFunction

data TypeMismatch = TypeMismatch

_Expression :: Prism' Value BULK
_Expression = prism' Expression extract
  where
    extract (Expression bulk) = Just bulk
    extract _ = Nothing

_Digest :: Prism' Value CheckDigest
_Digest = prism' Digest extract
  where
    extract (Digest digest) = Just digest
    extract _ = Nothing

_LazyFunction :: Prism' Value Core.LazyFunction
_LazyFunction = prism' LazyFunction extract
  where
    extract (LazyFunction lazyFunction) = Just lazyFunction
    extract _ = Nothing

data IncompleteNamespace = IncompleteNamespace
    { _namespaceDefinition :: NamespaceDefinition
    , _nextName :: Word8
    , _pendingDefinitions :: [(Word8, BULK)]
    }

makeLenses ''Scope
makeLenses ''IncompleteNamespace
