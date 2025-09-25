{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.BULK.Types where

import Data.ByteString.Lazy (ByteString)
import Witch (From (..))

data BULK
    = Nil
    | Form [BULK]
    | Array ByteString
    | Reference Namespace Int
    deriving (Eq, Ord, Show)

data Namespace
    = CoreNamespace
    | UnassociatedNamespace Int
    deriving (Eq, Ord, Show)

instance From Int Namespace where
    from 0x10 = CoreNamespace
    from ns = UnassociatedNamespace ns

instance From Namespace Int where
    from CoreNamespace = 0x10
    from (UnassociatedNamespace ns) = ns

instance Enum Namespace where
    toEnum = from
    fromEnum = from
