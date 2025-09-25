{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Data.BULK.Encode (encode, encodeNat, pattern Nat, encodeExpr, unsafeEncodeBounded, boundedPutter)
where

import Data.BULK.Decode (BULK (..), toNat)
import Data.Binary (Put, Word16, Word32, Word64, Word8, putWord8)
import Data.Binary.Put (putWord16be, putWord32be, putWord64be, runPut)
import Data.Bits (Bits (..))
import Data.ByteString.Builder as BB
import Data.ByteString.Lazy qualified as BS
import Data.Digits (digits)
import Data.Foldable (find, traverse_)
import Data.List.Extra (list)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))

encode :: [BULK] -> BS.ByteString
encode = BB.toLazyByteString . encodeSeq

encodeSeq :: [BULK] -> BB.Builder
encodeSeq = foldMap encodeExpr

encodeExpr :: BULK -> BB.Builder
encodeExpr Nil = BB.word8 0
encodeExpr (Form exprs) = BB.word8 1 <> encodeSeq exprs <> BB.word8 2
encodeExpr (Array [num]) | num < 64 = BB.word8 $ 0x80 + num
encodeExpr (Array bs) =
    if len < 64
        then BB.word8 (fromIntegral $ 0xC0 + len) <> BB.lazyByteString bs
        else BB.word8 3 <> encodeExpr (encodeNat len) <> BB.lazyByteString bs
  where
    len = BS.length bs
encodeExpr (Reference ns name)
    | ns < 0x7F = int ns <> int name
    | otherwise = foldMap int $ cutInWords ns ++ [name]

encodeNat :: (Integral a, Bits a) => a -> BULK
encodeNat = unsafeEncodeBounded unsafePutWord64s natEncoders

pattern Nat :: (Integral a, Bits a) => a -> BULK
pattern Nat num <- (toNat -> Just num)
    where
        Nat num = encodeNat num

unsafeEncodeBounded :: (Integral a) => (a -> Put) -> [BoundedPutter a] -> a -> BULK
unsafeEncodeBounded unsafePutter boundedPutters num = Array $ runPut putter
  where
    putter = fromMaybe (unsafePutter num) $ findPutter boundedPutters num

type BoundedPutter a = (a, a, a -> Put)

natEncoders :: (Integral a) => [BoundedPutter a]
natEncoders =
    [ boundedPutter putWord8
    , boundedPutter putWord16be
    , boundedPutter putWord32be
    , boundedPutter putWord64be
    ]

findPutter :: forall a. (Integral a) => [BoundedPutter a] -> a -> Maybe Put
findPutter encoders num =
    apply <$> find matchBounds encoders
  where
    matchBounds :: (a, a, b) -> Bool
    matchBounds (min_, max_, _) = num >= min_ && num <= max_
    apply (_, _, f) = f num

boundedPutter :: forall a b. (Integral a, Bounded a, Integral b) => (a -> Put) -> BoundedPutter b
boundedPutter putter = (fromIntegral (minBound @a), fromIntegral (maxBound @a), putter . fromIntegral)

asWords :: (Integral a) => a -> [Word8]
asWords num =
    list [0] (:) $ map fromIntegral (digits 256 num)

unsafePutWord64s :: (Integral a, Bits a) => a -> Put
unsafePutWord64s =
    traverse_ putWord64be . list [0] (:) . map fromIntegral . reverse . go
  where
    go 0 = []
    go n = (n .&. 0xFFFF_FFFF_FFFF_FFFF) : go (shiftR n 64)

cutInWords :: Int -> [Int]
cutInWords =
    cut 0x7F
  where
    cut remove remain =
        case (remain, remain >= remove) of
            (0, _) -> [0]
            (final, False) -> [final]
            (_, True) -> remove : cut 0xFF (remain - remove)

int :: Int -> BB.Builder
int = BB.word8 . fromIntegral
