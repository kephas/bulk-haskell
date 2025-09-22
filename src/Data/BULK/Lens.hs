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

class HasBytes a where
    _ByteString :: Prism' a ByteString

instance HasBytes ByteString where
    _ByteString = id

instance HasBytes [Word8] where
    _ByteString = prism' BL.unpack $ Just . BL.pack

instance HasBytes Text where
    _ByteString = prism' undefined $ eitherToMaybe . parseTextNotation

decoded :: Prism' ByteString BULK
decoded = prism' (encode . List.singleton) (eitherToMaybe . parseLazy getExpression)

_Bulk :: (HasBytes a) => Prism' a BULK
_Bulk = _ByteString . decoded
