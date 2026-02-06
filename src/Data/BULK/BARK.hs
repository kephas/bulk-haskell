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

hash0 :: NamespaceDefinition
hash0 =
    NamespaceDefinition
        { matchID = MatchNamePrefix 0x00 $ fromHex "411C77FD48C9C06DC7021C1CAB2FD70FD0749D4F2099BD8DE615000BDED43115"
        , mnemonic = "hash0"
        , names = [NameDefinition 0x00 "shake128" $ Digest CheckShake128]
        }

bark :: NamespaceDefinition
bark =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "6F410D8B538B23B69513B1E62401FB2BF5BFE5F5D90AD0D2FD5CF3FC3ADAC701"
        , mnemonic = "bark"
        , names = []
        }

fromHex :: Text -> LazyByteString
fromHex = fromStrict . fromJust . H.decodeHex
