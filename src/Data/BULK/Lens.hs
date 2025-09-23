{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.BULK.Lens where

import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (eitherToMaybe)
import Data.List qualified as List
import Data.Word (Word8)

import Data.BULK.Core (encodeInt, toIntegral)
import Data.BULK.Decode
import Data.BULK.Encode
import Data.BULK.TextNotation (parseTextNotation)
import Data.Bits (Bits)
import Data.Text (Text)

makePrisms ''BULK

_Nat :: (Integral a, Bits a) => Prism' BULK a
_Nat = prism' encodeNat toNat

_Int :: (Integral a) => Prism' BULK a
_Int = prism' encodeInt toIntegral

_Bytes :: Prism' Text ByteString
_Bytes = prism' undefined $ eitherToMaybe . parseTextNotation

_BulkExpr :: Prism' ByteString BULK
_BulkExpr = prism' (encode . List.singleton) (eitherToMaybe . parseLazy getExpression)
