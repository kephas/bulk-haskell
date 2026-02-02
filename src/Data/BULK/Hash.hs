{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.Hash where

import Crypto.Hash (SHAKE128 (..), hashWith)
import Data.ByteArray (ByteArrayAccess, eq, length, takeView)
import Data.ByteString (ByteString, null, toStrict)
import Data.ByteString.Lazy (LazyByteString)
import Data.String.Interpolate (i)
import Prelude hiding (length, null)

import Data.BULK.Types (CheckDigest (..))

newtype Digest = Digest ByteString
newtype Content = Content ByteString

runCheckDigest :: CheckDigest -> LazyByteString -> LazyByteString -> Either String ()
runCheckDigest CheckShake128 ref content = shake128Digest (mkDigest ref) (mkContent content)

shake128Digest :: Digest -> Content -> Either String ()
shake128Digest (Digest referenceDigest) (Content content) =
    case (null referenceDigest, referenceDigest `isPrefixOf` contentDigest) of
        (True, _) -> Left [i|missing digest #{contentDigest}|]
        (False, True) -> Right ()
        (False, False) -> Left [i|expected digest #{referenceDigest} but got #{contentDigest}|]
  where
    contentDigest = hashWith (SHAKE128 :: SHAKE128 256) content

isPrefixOf :: (ByteArrayAccess b1, ByteArrayAccess b2) => b1 -> b2 -> Bool
b1 `isPrefixOf` b2 =
    b1 `eq` takeView b2 (length b1)

mkDigest :: LazyByteString -> Digest
mkDigest = Digest . toStrict

mkContent :: LazyByteString -> Content
mkContent = Content . toStrict
