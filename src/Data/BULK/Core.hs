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
import Data.Either.Extra (eitherToMaybe)
import Data.Int (Int64)
import Prelude hiding (False, True)

import Data.BULK.Decode (parseLazy, toNat)
import Data.BULK.Encode (boundedPutter, encodeNat, unsafeEncodeBounded)
import Data.BULK.Types (BULK (..), pattern Core)

pattern Version, Namespace, Package, Import, Define, True, False, UnsignedInt, SignedInt, MnemonicDef :: BULK
pattern Version = Core 0x00
pattern Import = Core 0x01
pattern Namespace = Core 0x02
pattern Package = Core 0x03
pattern Define = Core 0x04
pattern True = Core 0x0E
pattern False = Core 0x0F
pattern UnsignedInt = Core 0x13
pattern SignedInt = Core 0x14
pattern MnemonicDef = Core 0xE1

version :: Int -> Int -> BULK
version major minor =
    Form [Version, encodeNat major, encodeNat minor]

define :: BULK -> BULK -> BULK
define ref value =
    Form [Define, ref, value]

-- | Extract a signed integer from a raw BULK expression
toIntegral :: (Integral a) => BULK -> Maybe a
toIntegral bulk =
    case bulk of
        Array _ -> toNat bulk
        Form [UnsignedInt, array] -> toNat array
        Form [SignedInt, ArraySize 1 bs] -> int getInt8 bs
        Form [SignedInt, ArraySize 2 bs] -> int getInt16be bs
        Form [SignedInt, ArraySize 4 bs] -> int getInt32be bs
        Form [SignedInt, ArraySize 8 bs] -> int getInt64be bs
        Form [SignedInt, ArrayBlocks n bs] -> bigInt n bs
        _ -> Nothing
  where
    int get = eitherToMaybe . fmap fromIntegral . parseLazy get
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
encodeInt num = Form [SignedInt, unsafeEncodeBounded undefined [boundedPutter putInt8, boundedPutter putInt16be, boundedPutter putInt32be, boundedPutter putInt64be] num]
