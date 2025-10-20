{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Unique (Unique)
import Data.Word (Word8)
import Witch (From (..))

data BULK
    = Nil
    | Form [BULK]
    | Array ByteString
    | Reference Namespace Word8
    deriving (Eq, Ord, Show)

data Namespace
    = CoreNamespace
    | UnassociatedNamespace Int
    | AssociatedNamespace Int FullNamespaceDefinition
    deriving (Show)

data NamespaceDefinition
    = NamespaceDefinition
    { matchID :: BULK -> Bool
    , mnemonic :: Text
    , names :: [NameDefinition]
    }
data FullNamespaceDefinition
    = FullNamespaceDefinition
    { unique :: Unique
    , matchID :: BULK -> Bool
    , mnemonic :: Text
    , names :: [NameDefinition]
    }

data NameDefinition = SelfEval {marker :: Word8, mnemonic :: Text}

data MatchBULK = MatchBULK {match :: BULK -> Bool, expected :: Text}

instance Eq Namespace where
    CoreNamespace == CoreNamespace = True
    UnassociatedNamespace m1 == UnassociatedNamespace m2 = m1 == m2
    AssociatedNamespace m1 ns1 == AssociatedNamespace m2 ns2 = m1 == m2 && ns1 == ns2
    _ == _ = False

instance Ord Namespace where
    compare CoreNamespace CoreNamespace = EQ
    compare CoreNamespace _ = LT
    compare _ CoreNamespace = GT
    compare (UnassociatedNamespace m1) (UnassociatedNamespace m2) = compare m1 m2
    compare (UnassociatedNamespace _) (AssociatedNamespace _ _) = LT
    compare (AssociatedNamespace _ _) (UnassociatedNamespace _) = GT
    compare (AssociatedNamespace m1 _) (AssociatedNamespace m2 _) = compare m1 m2

instance Eq FullNamespaceDefinition where
    ns1 == ns2 = ns1.unique == ns2.unique

instance Show FullNamespaceDefinition where
    show (FullNamespaceDefinition{mnemonic}) = show mnemonic

instance Ord FullNamespaceDefinition where
    compare ns1 ns2 = compare ns1.unique ns2.unique

instance From Int Namespace where
    from 0x10 = CoreNamespace
    from ns = UnassociatedNamespace ns

instance From Namespace Int where
    from CoreNamespace = 0x10
    from (UnassociatedNamespace ns) = ns
    from (AssociatedNamespace ns _) = ns

instance Enum Namespace where
    toEnum = from
    fromEnum = from
