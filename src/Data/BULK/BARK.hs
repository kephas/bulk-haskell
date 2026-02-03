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
        { matchID = MatchNamePrefix 0x00 $ fromHex "E2ECDA494A78195B07034AB10B2C4390C94C01251B13D1A483D21E8B781D703D"
        , mnemonic = "hash0"
        , names = [DigestName 0x00 "shake128" CheckShake128]
        }

bark :: NamespaceDefinition
bark =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "0066F0235D80F6EA9336AD97A656A0A96289FAA130C82FE1D0E74CB30B702EB1"
        , mnemonic = "bark"
        , names = []
        }

fromHex :: Text -> LazyByteString
fromHex = fromStrict . fromJust . H.decodeHex
