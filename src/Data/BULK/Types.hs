{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Word (Word8)
import Witch (From (..), TryFrom (..), maybeTryFrom)

data BULK
    = Nil
    | Form [BULK]
    | Array ByteString
    | Reference Name
    deriving (Eq, Ord, Show)

data Name = Name Namespace Word8 deriving (Eq, Ord, Show)

data Namespace
    = CoreNamespace
    | UnassociatedNamespace Int
    | AssociatedNamespace NamespaceDefinition
    deriving (Eq, Ord, Show)

pattern Core :: Word8 -> BULK
pattern Core name = (Reference (Name CoreNamespace name))

data NamespaceDefinition
    = NamespaceDefinition
    { matchID :: MatchID
    , mnemonic :: Text
    , names :: [NameDefinition]
    }
    deriving (Eq, Ord, Show)

data NameDefinition = NameDefinition
    { marker :: Word8
    , mnemonic :: Text
    , value :: Value
    }
    deriving (Eq, Ord, Show)

data Value = SelfEval | Expression BULK | Digest CheckDigest | LazyFunction LazyFunction deriving (Eq, Ord, Show)

data CheckDigest = CheckShake128 deriving (Eq, Ord, Show)

data LazyFunction = Version | Import | Define | Mnemonic | DefineMnemonic
    deriving (Eq, Ord, Show)

data Package = Package
    { matchID :: MatchID
    , nsIDs :: [Maybe NamespaceDefinition]
    }
    deriving (Eq, Ord, Show)

data MatchID
    = MatchNone
    | MatchEq BULK
    | MatchNamePrefix Word8 ByteString
    | MatchQualifiedNamePrefix Name ByteString
    deriving (Eq, Ord, Show)

data MatchBULK = MatchBULK {match :: BULK -> Bool, expected :: Text}

newtype Context = Context {scope :: Scope} deriving (Show)

data Scope = Scope
    { _associatedNamespaces :: M.Map Int NamespaceDefinition
    , _definitions :: M.Map Name Value
    , _knownNamespaces :: S.Set NamespaceDefinition
    , _lastingNamespaces :: S.Set NamespaceDefinition
    , _knownPackages :: S.Set Package
    , _definingNamespace :: Maybe IncompleteNamespace
    }
    deriving (Show)

data TypeMismatch = TypeMismatch

data IncompleteNamespace = IncompleteNamespace
    { _namespaceDefinition :: NamespaceDefinition
    , _nextName :: Word8
    }
    deriving (Show)

instance From Int Namespace where
    from 0x10 = CoreNamespace
    from ns = UnassociatedNamespace ns

instance TryFrom Namespace Int where
    tryFrom = maybeTryFrom \case
        CoreNamespace -> Just 0x10
        UnassociatedNamespace ns -> Just ns
        AssociatedNamespace _ -> Nothing
