{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

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
    | Reference Ref
    deriving (Eq, Ord, Show)

data Ref = Ref {nsID :: NamespaceID, name :: Name} deriving (Eq, Ord, Show)

pattern Core :: Word8 -> BULK
pattern Core name <- (Reference (Ref CoreNS Name{marker = name}))
    where
        Core name = Reference $ Ref CoreNS $ from name

data Namespace
    = Namespace
    { matchID :: NamespaceID
    , mnemonic :: Text
    , names :: [Name]
    }
    deriving (Ord, Show)

data Name = Name
    { marker :: Word8
    , mnemonic :: Maybe Text
    , value :: Value
    }
    deriving (Eq, Ord, Show)

data Value = SelfEval | Expression BULK | Digest CheckDigest | LazyFunction LazyFunction deriving (Eq, Ord, Show)

data CheckDigest = CheckShake128 deriving (Eq, Ord, Show)

data LazyFunction = Version | Import | Define | Mnemonic | Trace
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
    | MatchQualifiedNamePrefix Ref ByteString
    deriving (Eq, Ord, Show)

data MatchBULK = MatchBULK {match :: BULK -> Bool, expected :: Text}

data Context = Context {packages :: S.Set Package, namespaces :: S.Set Namespace}
    deriving (Eq, Ord, Show)

data Scope = Scope
    { _associatedNamespaces :: M.Map Int NamespaceID
    , _knownNamespaces :: M.Map NamespaceID Namespace
    , _lastingNamespaces :: S.Set NamespaceID
    , _knownPackages :: S.Set Package
    }
    deriving (Show)

data TypeMismatch = TypeMismatch

newtype Warning = Warning {unWarning :: String}

data Representation = BinaryBULK | TextBULK deriving (Eq, Show)

newtype Result a = Result {unResult :: Either String a}
    deriving newtype (Eq, Ord, Show, Functor, Applicative, Monad)

instance MonadFail Result where
    fail = Result . Left

class WithKey k a | a -> k where
    getKey :: a -> k

instance WithKey Word8 Name where
    getKey = (.marker)

instance WithKey NamespaceID Namespace where
    getKey = (.matchID)

withKey :: (WithKey k a) => a -> (k, a)
withKey x = (getKey x, x)

instance Eq Namespace where
    ns1 == ns2 = ns1.matchID == ns2.matchID

instance Semigroup Context where
    ctx1@Context{} <> ctx2@Context{} = Context{packages = S.union ctx1.packages ctx2.packages, namespaces = S.union ctx1.namespaces ctx2.namespaces}

instance From Int NamespaceID where
    from 0x10 = CoreNS
    from ns = UnassociatedNS ns

instance TryFrom NamespaceID Int where
    tryFrom = maybeTryFrom \case
        CoreNS -> Just 0x10
        UnassociatedNS ns -> Just ns
        _ -> Nothing

instance From Word8 Name where
    from num = Name num Nothing SelfEval

instance From Context Scope where
    from ctx =
        Scope
            { _associatedNamespaces = M.empty
            , _knownNamespaces = M.fromList $ map withKey $ S.elems ctx.namespaces
            , _lastingNamespaces = S.empty
            , _knownPackages = ctx.packages
            }

instance From Scope Context where
    from scope = Context{packages = scope._knownPackages, namespaces = S.fromList $ M.elems scope._knownNamespaces}
