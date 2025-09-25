{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.BULK.Lens where

import Control.Lens
import Data.Bits (Bits)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Extra (eitherToMaybe)
import Data.List qualified as List
import Data.Text (Text)

import Data.BULK.Core (encodeInt, toIntegral)
import Data.BULK.Decode
import Data.BULK.Encode
import Data.BULK.TextNotation (parseTextNotation)
import Data.BULK.Types

makePrisms ''BULK

-- | This 'Prism' provides a 'Traversal' for tweaking the natural number encoded as a BULK array
_Nat :: (Integral a, Bits a) => Prism' BULK a
_Nat = prism' encodeNat toNat

-- | This 'Prism' provides a 'Traversal' for tweaking the integer encoded as a BULK expression
_Int :: (Integral a) => Prism' BULK a
_Int = prism' encodeInt toIntegral

_Bytes :: Prism' Text ByteString
_Bytes = prism' undefined $ eitherToMaybe . parseTextNotation

-- | This 'Prism' provides a 'Traversal' for tweaking the yield of a BULK stream encoding a single expression
_BulkExpr :: Prism' ByteString BULK
_BulkExpr = prism' (encode . List.singleton) (eitherToMaybe . parseLazy getExpression)

-- | This 'Prism' provides a 'Traversal' for tweaking the yield of a BULK stream
bulkStreamL :: VersionConstraint -> Prism' ByteString BULK
bulkStreamL constraint = prism' (encode . forceList) (eitherToMaybe . parseLazy (getStream constraint))
  where
    forceList (Form list) = list
    forceList bulk = [bulk]

-- | This 'Prism' provides a 'Traversal' for tweaking the yield of a BULK 1.0 stream (that doesn't need to contain a version form)
_Bulk :: Prism' ByteString BULK
_Bulk = bulkStreamL $ SetVersion 1 0
