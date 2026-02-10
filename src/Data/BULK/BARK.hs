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
        { matchID = MatchNamePrefix 0x00 $ fromHex "38E4420105EB3E54B9CBFC72E3A89685B4A3B3BA74BA3DD4D91DE2727C1A6163"
        , mnemonic = "hash0"
        , names = [NameDefinition 0x00 "shake128" $ Digest CheckShake128]
        }

bark :: NamespaceDefinition
bark =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "EFD301961A076800D4D19D9D87A1DE1DEA5B305F1D630BFDFF64CBB9DD763EE1"
        , mnemonic = "bark"
        , names = []
        }

fromHex :: Text -> LazyByteString
fromHex = fromStrict . fromJust . H.decodeHex
