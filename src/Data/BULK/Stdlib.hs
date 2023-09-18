module Data.BULK.Stdlib (Bulk32, version) where

data Bulk32 = Version Int Int deriving (Eq, Show)

version :: Int -> Int -> Bulk32
version = Version
