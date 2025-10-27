{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Data.BULK.Core where

import Data.Binary.Get (getInt16be, getInt32be, getInt64be, getInt8)
import Data.Binary.Put (putInt16be, putInt32be, putInt64be, putInt8)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)

import Data.BULK.Decode (parseLazy, toNat)
import Data.BULK.Encode (boundedPutter, encodeNat, unsafeEncodeBounded)
import Data.BULK.Types (BULK (..), Namespace (CoreNamespace))
import Data.Word (Word8)
import Vary.Extra (veitherToMaybe)

version :: Int -> Int -> BULK
version major minor =
    Form [Core 0x00, encodeNat major, encodeNat minor]

define :: BULK -> BULK -> BULK
define ref value =
    Form [Core 0x06, ref, value]

pattern Core :: Word8 -> BULK
pattern Core name = (Reference CoreNamespace name)

-- | Extract a signed integer from a raw BULK expression
toIntegral :: (Integral a) => BULK -> Maybe a
toIntegral bulk =
    case bulk of
        Array _ -> toNat bulk
        Form [Core 0x20, array] -> toNat array
        Form [Core 0x21, ArraySize 1 bs] -> int getInt8 bs
        Form [Core 0x21, ArraySize 2 bs] -> int getInt16be bs
        Form [Core 0x21, ArraySize 4 bs] -> int getInt32be bs
        Form [Core 0x21, ArraySize 8 bs] -> int getInt64be bs
        Form [Core 0x21, ArrayBlocks n bs] -> bigInt n bs
        _ -> Nothing
  where
    int get = veitherToMaybe . fmap fromIntegral . parseLazy get
    bigInt blocks = int (getBlocks blocks 0)
    getBlocks 1 acc = (acc +) . fromIntegral <$> getInt64be
    getBlocks blocks _acc = do
        nextBlock <- getInt64be
        getBlocks (blocks - 1) $ 2 ^ 64 * fromIntegral nextBlock

pattern ArraySize :: Int64 -> BL.ByteString -> BULK
pattern ArraySize size array <- (sized -> Just (size, array))

pattern ArrayBlocks :: Int64 -> BL.ByteString -> BULK
pattern ArrayBlocks blocks array <- (blockSized -> Just (blocks, array))

sized :: BULK -> Maybe (Int64, BL.ByteString)
sized (Array bs) = Just (BL.length bs, bs)
sized _ = Nothing

blockSized :: BULK -> Maybe (Int64, BL.ByteString)
blockSized (Array bs) =
    if aligned then Just (blocks, bs) else Nothing
  where
    (blocks, aligned) = (== 0) <$> divMod (BL.length bs) 8
blockSized _ = Nothing

encodeInt :: (Integral a) => a -> BULK
encodeInt num = Form [Reference CoreNamespace 0x21, unsafeEncodeBounded undefined [boundedPutter putInt8, boundedPutter putInt16be, boundedPutter putInt32be, boundedPutter putInt64be] num]
