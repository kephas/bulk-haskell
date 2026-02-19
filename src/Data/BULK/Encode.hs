{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Data.BULK.Encode (encode, pattern IntReference, encodeNat, pattern Nat, encodeExpr, unsafeEncodeBounded, boundedPutter)
where

import Data.Binary (Put, putWord8)
import Data.Binary.Put (putWord16be, putWord32be, putWord64be, runPut)
import Data.Bits (Bits (..))
import Data.ByteString.Builder as BB
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (find, traverse_)
import Data.List.Extra (list)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Word (Word8)
import Witch (from, tryFrom)

import Data.BULK.Debug (debug)
import Data.BULK.Decode (toNat)
import Data.BULK.Types (BULK (..), Name (..), Ref (..))

encode :: [BULK] -> Either String BS.ByteString
encode = (BB.toLazyByteString <$>) . encodeSeq

encodeSeq :: [BULK] -> Either String BB.Builder
encodeSeq = (mconcat <$>) . traverse encodeExpr

encodeExpr :: BULK -> Either String BB.Builder
encodeExpr Nil = Right $ BB.word8 0
encodeExpr (Form exprs) = do
    content <- encodeSeq exprs
    Right $ BB.word8 1 <> content <> BB.word8 2
encodeExpr (Array [num]) | num < 64 = Right $ BB.word8 $ 0x80 + num
encodeExpr (Array bs) =
    if len < 64
        then Right $ BB.word8 (fromIntegral $ 0xC0 + len) <> BB.lazyByteString bs
        else do
            size <- encodeExpr (encodeNat len)
            Right $ BB.word8 3 <> size <> BB.lazyByteString bs
  where
    len = BS.length bs
encodeExpr (IntReference ns name)
    | ns < 0x7F = Right $ int ns <> BB.word8 name
    | otherwise = Right $ foldMap int $ cutInWords ns ++ [fromIntegral name]
encodeExpr (Reference ref) =
    Left [i|not an encodable reference: #{debug ref}|]

pattern IntReference :: Int -> Word8 -> BULK
pattern IntReference ns num <- (toIntRef -> Just (ns, num))
    where
        IntReference ns num = Reference $ Ref (from ns) $ from num

toIntRef :: BULK -> Maybe (Int, Word8)
toIntRef (Reference (Ref ns name)) = either (const Nothing) (\marker -> Just (marker, name.marker)) $ tryFrom ns
toIntRef _ = Nothing

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
