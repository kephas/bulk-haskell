{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.BULK.Encode where

import Control.Lens ((%~), (&))
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)

import Data.BULK (BULK (..), encodeNat, _Nat, pattern Nat)
import Data.BULK.Types (pattern Core)

bulkNum :: Word8 -> ByteString -> BULK
bulkNum refName bytes = Form [Core refName, Array bytes]

instance Num BULK where
    (+) = nat2 (+)
    (-) = nat2 (-)
    (*) = nat2 (*)
    abs = nat1 abs
    signum = nat1 signum
    negate = nat1 negate
    fromInteger = encodeNat

nat2 :: (Integer -> Integer -> Integer) -> BULK -> BULK -> BULK
nat2 op (Nat x) (Nat y) = Nat $ op x y
nat2 _op bulk1 _bulk2 = bulk1

nat1 :: (Integer -> Integer) -> BULK -> BULK
nat1 f bulk = bulk & _Nat %~ f
