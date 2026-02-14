{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.BULK.Lens where

import Control.Lens (Lens', Prism', Traversal', at, ix, lens, makeLenses, makePrisms, prism', (^?!))
import Data.Bits (Bits)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Extra (eitherToMaybe)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Hex qualified as H

import Data.BULK.Core (encodeInt, toIntegral)
import Data.BULK.Decode (VersionConstraint (Version1), getExpression, getStream, parseLazy, toNat)
import Data.BULK.Encode (encode, encodeNat)
import Data.BULK.TextNotation (parseNotation)
import Data.BULK.Types (BULK (Form), CheckDigest, Context, Name (..), Namespace (..), NamespaceID, Ref (..), Scope, Value (..), withKey)
import Data.BULK.Types qualified as Core
import Data.Map.Strict qualified as M
import Data.Word (Word8)

makePrisms ''BULK
makePrisms ''Context
makeLenses ''Scope

-- | This 'Prism' provides a 'Traversal' for tweaking the natural number encoded as a BULK array
_Nat :: (Integral a, Bits a) => Prism' BULK a
_Nat = prism' encodeNat toNat

-- | This 'Prism' provides a 'Traversal' for tweaking the integer encoded as a BULK expression
_Int :: (Integral a) => Prism' BULK a
_Int = prism' encodeInt toIntegral

_Bytes :: Prism' Text ByteString
_Bytes = prism' undefined $ eitherToMaybe . parseNotation

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
_Bulk = bulkStreamL Version1

-- | This 'Prism` provides a 'Traversal' for tweaking the content of a 'ByteString'
_Hex :: Prism' ByteString Text
_Hex = prism' (H.lazyByteString . fromMaybe "" . H.decodeHex) (Just . H.strictText . H.lazilyEncodeHex)

_Expression :: Prism' Value BULK
_Expression = prism' Expression extract
  where
    extract (Expression bulk) = Just bulk
    extract _ = Nothing

_Digest :: Prism' Value CheckDigest
_Digest = prism' Digest extract
  where
    extract (Digest digest) = Just digest
    extract _ = Nothing

_LazyFunction :: Prism' Value Core.LazyFunction
_LazyFunction = prism' LazyFunction extract
  where
    extract (LazyFunction lazyFunction) = Just lazyFunction
    extract _ = Nothing

coreName :: BULK -> Text -> Value -> Name
coreName ref mnemonic' value =
    Name{..}
  where
    marker = (ref ^?! _Reference).name.marker
    mnemonic = Just mnemonic'

knownNS :: NamespaceID -> Traversal' Scope Namespace
knownNS nsId = knownNamespaces . ix nsId

type NameMap = M.Map Word8 Name

nameMap :: Lens' Namespace NameMap
nameMap =
    lens getMap setMap
  where
    getMap =
        M.fromList . map withKey . (.names)
    setMap nsDef nsMap =
        nsDef{names = M.elems nsMap}

nsName :: Word8 -> Traversal' Namespace Name
nsName name = nameMap . ix name

setNsName :: Word8 -> Traversal' Namespace (Maybe Name)
setNsName name = nameMap . at name
