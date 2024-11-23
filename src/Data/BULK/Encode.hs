{-# LANGUAGE OverloadedLists #-}

module Data.BULK.Encode (encode, encodeInt)
where

import Data.BULK.Decode (BULK (..))
import Data.ByteString.Lazy qualified as BS
import Data.Word (Word8)

encode :: [BULK] -> [Word8]
encode = foldr (\expr -> (encodeExpr expr ++)) []

encodeExpr :: BULK -> [Word8]
encodeExpr Nil = [0]
encodeExpr (Form exprs) = [1] ++ encode exprs ++ [2]
encodeExpr (Array bs) =
    if len < 64
        then fromIntegral (0xC0 + len) : BS.unpack bs
        else []
  where
    len = BS.length bs

encodeInt :: Int -> BULK
encodeInt 0 = Array [0]
