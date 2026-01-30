{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Eval.Types where

import Control.Lens (Prism', makeLenses, makePrisms, prism')
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Word (Word8)

import Data.BULK.Types (BULK (..), CheckDigest, Name, NamespaceDefinition (..), Package (..))
import Data.BULK.Types qualified as Core (LazyFunction (..))

newtype Context = Context Scope deriving (Show)

data Scope = Scope
    { _associatedNamespaces :: M.Map Int NamespaceDefinition
    , _definitions :: M.Map Name Value
    , _knownNamespaces :: S.Set NamespaceDefinition
    , _lastingNamespaces :: S.Set NamespaceDefinition
    , _knownPackages :: S.Set Package
    , _definingNamespace :: Maybe IncompleteNamespace
    }
    deriving (Show)

data Value = Expression BULK | Digest CheckDigest | LazyFunction Core.LazyFunction deriving (Show)

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
    deriving (Show)

makePrisms ''Context
makeLenses ''Scope
makeLenses ''IncompleteNamespace
