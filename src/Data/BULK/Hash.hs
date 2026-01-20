{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.Hash where

import Crypto.Hash (SHAKE128 (..), hashWith)
import Data.BULK.Types (NameDefinition (..))
import Data.ByteArray (ByteArrayAccess, eq, length, takeView)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Lazy (LazyByteString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Word (Word8)
import Prelude hiding (length)

newtype Digest = Digest ByteString
newtype Content = Content ByteString

shake128Name :: Word8 -> Text -> NameDefinition
shake128Name marker mnemonic =
    DigestName{..}
  where
    checkDigest ref content = shake128Digest (mkDigest ref) (mkContent content)

shake128Digest :: Digest -> Content -> Either String ()
shake128Digest (Digest referenceDigest) (Content content) =
    if referenceDigest `isPrefixOf` contentDigest
        then
            Right ()
        else
            Left [i|expected digest #{referenceDigest} but got #{contentDigest}|]
  where
    contentDigest = hashWith (SHAKE128 :: SHAKE128 256) content

isPrefixOf :: (ByteArrayAccess b1, ByteArrayAccess b2) => b1 -> b2 -> Bool
b1 `isPrefixOf` b2 =
    b1 `eq` takeView b2 (length b1)

mkDigest :: LazyByteString -> Digest
mkDigest = Digest . toStrict

mkContent :: LazyByteString -> Content
mkContent = Content . toStrict
