{-# OPTIONS_GHC -Wno-orphans #-}

module Test.BULK.Encode where

import Data.BULK (BULK (..))
import Data.ByteString.Lazy (ByteString)

core :: Int -> BULK
core = Reference 16

bulkNum :: Int -> ByteString -> BULK
bulkNum refName bytes = Form [Reference 0x10 refName, Array bytes]
