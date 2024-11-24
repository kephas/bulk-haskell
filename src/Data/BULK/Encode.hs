{-# LANGUAGE OverloadedLists #-}

module Data.BULK.Encode (encode, encodeInt)
where

import Data.BULK.Decode (BULK (..))
import Data.ByteString.Lazy qualified as BS
import Data.Digits (digits)
import Data.Word (Word8)

encode :: [BULK] -> [Word8]
encode = foldr (\expr -> (encodeExpr expr ++)) []

encodeExpr :: BULK -> [Word8]
encodeExpr Nil = [0]
encodeExpr (Form exprs) = [1] ++ encode exprs ++ [2]
encodeExpr (Array bs) =
    if len < 64
        then fromIntegral (0xC0 + len) : BS.unpack bs
        else [3] ++ encodeExpr (encodeInt $ fromIntegral len) ++ BS.unpack bs
  where
    len = BS.length bs
encodeExpr (Reference ns name)
    | ns < 0x7F = map fromIntegral [ns, name]
    | otherwise = cutInWords ns ++ [fromIntegral name]

encodeInt :: (Integral a) => a -> BULK
encodeInt = Array . BS.pack . asWords

asWords :: (Integral a) => a -> [Word8]
asWords num =
    if null words
        then [0]
        else words
  where
    words = map fromIntegral $ digits 256 num

cutInWords :: (Integral a) => a -> [Word8]
cutInWords num =
    map fromIntegral $ cut 0x7F num
  where
    cut remove remain =
        case (remain, remain >= remove) of
            (0, _) -> [0]
            (final, False) -> [final]
            (_, True) -> remove : cut 0xFF (remain - remove)
