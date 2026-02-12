{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.BARK where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString, fromStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Text.Hex qualified as H
import Prelude hiding (words)

import Data.BULK

data BARK = Description {path :: FilePath, shake128 :: ByteString}
    deriving (Eq, Show)

instance FromBULK BARK where
    parseBULK =
        bark <*:> "description" $
            bark <:> "metadata" $ do
                path <- bark <:> "about" $ do
                    bark <:> "path" $ string
                shake128 <- bark <:> "hash" $ nextBULK
                pure Description{..}

hash0 :: Namespace
hash0 =
    Namespace
        { matchID = MatchNamePrefix 0x00 $ fromHex "99FE9CBED1B3F0D34869530AA1E6A8AE699C8954714A29696DA4386AC7B7B487"
        , mnemonic = "hash0"
        , names = [NameDefinition 0x00 "shake128" $ Digest CheckShake128]
        }

bark :: Namespace
bark =
    Namespace
        { matchID = MatchQualifiedNamePrefix (Name hash0.matchID 0x00) $ fromHex "A3AB0C21DE5AD45685D159AFD1A051FA78128B03A547F63E2A836E5C2DEC3551"
        , mnemonic = "bark"
        , names = []
        }

fromHex :: Text -> LazyByteString
fromHex = fromStrict . fromJust . H.decodeHex
