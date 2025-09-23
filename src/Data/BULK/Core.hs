{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.BULK.Core where

import Data.BULK.Decode (BULK (Array, Form, Reference), parseLazy, toNat)
import Data.BULK.Encode (boundedPutter, encodeNat, unsafeEncodeBounded)
import Data.Binary.Get (getInt16be, getInt32be, getInt64be, getInt8)
import Data.Binary.Put (putInt16be, putInt32be, putInt64be, putInt8)
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (eitherToMaybe)

version :: Int -> Int -> BULK
version major minor =
    Form [Reference 16 0, encodeNat major, encodeNat minor]

define :: BULK -> BULK -> BULK
define ref value =
    Form [Reference 16 9, ref, value]

-- | Extract a signed integer from a raw BULK expression
toIntegral :: (Integral a) => BULK -> Maybe a
toIntegral bulk =
    case bulk of
        Array _ -> toNat bulk
        Form [Reference 16 0x20, array] -> toNat array
        Form [Reference 16 0x21, sized -> ArraySize 1 bs] -> int getInt8 bs
        Form [Reference 16 0x21, sized -> ArraySize 2 bs] -> int getInt16be bs
        Form [Reference 16 0x21, sized -> ArraySize 4 bs] -> int getInt32be bs
        Form [Reference 16 0x21, sized -> ArraySize 8 bs] -> int getInt64be bs
        _ -> Nothing
  where
    int get = eitherToMaybe . fmap fromIntegral . parseLazy get

pattern ArraySize size array <- Just (size, array)

sized (Array bs) = Just (BL.length bs, bs)
sized _ = Nothing

encodeInt :: (Integral a) => a -> BULK
encodeInt num = Form [Reference 0x10 0x21, unsafeEncodeBounded undefined [boundedPutter putInt8, boundedPutter putInt16be, boundedPutter putInt32be, boundedPutter putInt64be] num]
