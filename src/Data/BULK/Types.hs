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
    | Reference {name :: Name, mnemonic :: Maybe Text}
    deriving (Eq, Ord, Show)

data Name = Name NamespaceID Word8 deriving (Eq, Ord, Show)

pattern Core :: Word8 -> BULK
pattern Core name <- (Reference (Name CoreNS name) _)
    where
        Core name = Reference (Name CoreNS name) Nothing

data Namespace
    = Namespace
    { matchID :: NamespaceID
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

data LazyFunction = Version | Import | Define | Mnemonic
    deriving (Eq, Ord, Show)

data Package = Package
    { matchID :: NamespaceID
    , nsIDs :: [Maybe NamespaceID]
    }
    deriving (Eq, Ord, Show)

data NamespaceID
    = CoreNS
    | TempNS
    | UnassociatedNS Int
    | MatchEq BULK
    | MatchNamePrefix Word8 ByteString
    | MatchQualifiedNamePrefix Name ByteString
    deriving (Eq, Ord, Show)

data MatchBULK = MatchBULK {match :: BULK -> Bool, expected :: Text}

newtype Context = Context {scope :: Scope} deriving (Show)

data Scope = Scope
    { _associatedNamespaces :: M.Map Int NamespaceID
    , _definitions :: M.Map Name Value
    , _knownNamespaces :: M.Map NamespaceID Namespace
    , _lastingNamespaces :: S.Set NamespaceID
    , _knownPackages :: S.Set Package
    }
    deriving (Show)

data TypeMismatch = TypeMismatch

instance From Int NamespaceID where
    from 0x10 = CoreNS
    from ns = UnassociatedNS ns

instance TryFrom NamespaceID Int where
    tryFrom = maybeTryFrom \case
        CoreNS -> Just 0x10
        UnassociatedNS ns -> Just ns
        _ -> Nothing
