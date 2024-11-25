{-# LANGUAGE OverloadedLists #-}

module Data.BULK.Encode (encode, encodeInt)
where

import Data.BULK.Decode (BULK (..))
import Data.ByteString.Builder as BB
import Data.ByteString.Lazy qualified as BS
import Data.Digits (digits)
import Data.Word (Word8)

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
        else BB.word8 3 <> encodeExpr (encodeInt len) <> BB.lazyByteString bs
  where
    len = BS.length bs
encodeExpr (Reference ns name)
    | ns < 0x7F = int ns <> int name
    | otherwise = foldMap int $ cutInWords ns ++ [name]

encodeInt :: (Integral a) => a -> BULK
encodeInt = Array . BS.pack . asWords

asWords :: (Integral a) => a -> [Word8]
asWords num =
    if null words
        then [0]
        else words
  where
    words = map fromIntegral $ digits 256 num

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
