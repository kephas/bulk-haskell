{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.BULK.Encode (encode, encodeNat, pattern Nat, encodeExpr, unsafeEncodeBounded, boundedPutter)
where

import Data.Binary (Put, putWord8)
import Data.Binary.Put (putWord16be, putWord32be, putWord64be, runPut)
import Data.Bits (Bits (..))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (find, fold, traverse_)
import Data.List.Extra (list)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Word (Word8)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

import Data.BULK.Debug (debug)
import Data.BULK.Decode (toNat)
import Data.BULK.Types (BULK (..), Name (..), NamespaceID (..), Ref (..))

encode :: (Member (Error String) r) => [BULK] -> Sem r BS.ByteString
encode = (BB.toLazyByteString <$>) . encodeSeq

encodeSeq :: (Member (Error String) r) => [BULK] -> Sem r BB.Builder
encodeSeq = (mconcat <$>) . traverse encodeExpr

encodeExpr :: (Member (Error String) r) => BULK -> Sem r BB.Builder
encodeExpr Nil = pure $ BB.word8 0
encodeExpr (Form exprs) = do
    content <- encodeSeq exprs
    pure $ BB.word8 1 <> content <> BB.word8 2
encodeExpr (Array bs)
    | Just num <- smallInt bs =
        pure $ BB.word8 $ 0x80 + num
    | len < 64 =
        pure $ BB.word8 (fromIntegral $ 0xC0 + len) <> BB.lazyByteString bs
    | otherwise = do
        size <- encodeExpr (encodeNat len)
        pure $ BB.word8 3 <> size <> BB.lazyByteString bs
  where
    len = BS.length bs
encodeExpr ref@(Reference (Ref ns name))
    | Just num <- numNS ns =
        pure $ encodeNS num <> BB.word8 name.index
    | otherwise =
        throw [i|not an encodable reference: #{debug ref}|]

smallInt :: BS.LazyByteString -> Maybe Word8
smallInt [num] = if num < 64 then Just num else Nothing
smallInt _words = Nothing

numNS :: NamespaceID -> Maybe Int
numNS CoreNS = Just 0x10
numNS (UnassociatedNS num) = Just num
numNS _ns = Nothing

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

encodeNS :: Int -> BB.Builder
encodeNS = fold . cut 0x7F
  where
    cut remove remain =
        case (remain, remain < remove) of
            (final, True) -> [int8 final]
            (_, False) -> int8 remove : cut 0xFF (remain - remove)
    int8 = BB.word8 . fromIntegral
