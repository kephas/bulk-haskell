{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.BARK where

import Crypto.Hash qualified as Hash
import Data.ByteArray (ByteArrayAccess, convert, eq)
import Data.ByteString as BS (StrictByteString)
import Data.List (uncons)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import System.FilePath (takeDirectory, (</>))
import Prelude hiding (words)

import Data.BULK (CheckDigest (..), Context, FromBULK (..), Name (..), Namespace (..), NamespaceID (..), Ref (..), Value (..), hex, list, nextBULK, string, withFormCase, (.:), (<*:>), (<:>))
import Data.BULK.API (runAllIO)
import Data.BULK.Debug (debug)
import Data.BULK.ToFrom (decodeFile)
import Data.BULK.Utils (IOE, readFileBS)

newtype BARK = BARK [Entry]
    deriving (Eq, Show)

data Entry = Description {path :: FilePath, hash :: Hash}
    deriving (Eq, Show)

data Hash
    = Shake128 StrictByteString
    | MD5 StrictByteString
    deriving (Eq, Show)

data Verification
    = OK {path :: FilePath}
    | Failed {path :: FilePath, reason :: String}
    deriving (Eq, Show)

verifyManifest :: Context -> FilePath -> IO (Either String [Verification])
verifyManifest ctx file = runAllIO do
    BARK entries <- decodeFile ctx file
    traverse (verifyManifestEntry $ takeDirectory file) entries

verifyManifestEntry :: FilePath -> Entry -> IOE r Verification
verifyManifestEntry root entry = do
    content <- readFileBS $ root </> entry.path
    let (entryDigest, fileDigest) = case entry.hash of
            Shake128 digest -> (digest, toBS $ Hash.hashWith (Hash.SHAKE128 :: Hash.SHAKE128 256) content)
            MD5 digest -> (digest, toBS $ Hash.hashWith Hash.MD5 content)
    pure $
        if entryDigest `eq` fileDigest
            then
                OK entry.path
            else
                Failed entry.path [i|expected digest #{debug entryDigest} but got #{debug fileDigest}|]

displayVerification :: Verification -> String
displayVerification OK{path} = [i|#{path}: ✅|]
displayVerification Failed{path, reason} = [i|#{path}: 🛑 (#{reason})|]

toBS :: (ByteArrayAccess ba) => ba -> StrictByteString
toBS = convert

instance FromBULK BARK where
    parseBULK =
        bark <*:> "bark" $ BARK <$> list

instance FromBULK Entry where
    parseBULK =
        bark <*:> "description" $
            bark <:> "metadata" $ do
                path <- bark <:> "about" $ do
                    bark <:> "path" $ string
                hash <- bark <:> "hash" $ nextBULK
                pure Description{..}

instance FromBULK Hash where
    parseBULK =
        withFormCase
            [ (bark .: "shake128", Shake128 <$> nextBULK)
            , (bark .: "md5", MD5 <$> nextBULK)
            ]

hash0 :: Namespace
hash0 =
    Namespace
        { matchID = MatchNamePrefix 0x00 [hex|9DBFD6029C1EBE32EC16749703A283DFC1B47C4E925473435529B5769FD89311|]
        , mnemonic = "hash0"
        , names = [Name 0x00 (Just "shake128") $ Digest CheckShake128]
        }

bark :: Namespace
bark =
    Namespace
        { matchID = MatchQualifiedNamePrefix (Ref hash0.matchID $ forceHead hash0.names) [hex|F83B2E34FC34C4C4AD942590FB1BE40B206D9275353BE75CA9B3835B28C48B2F|]
        , mnemonic = "bark"
        , names = []
        }

forceHead :: [a] -> a
forceHead = fst . fromJust . uncons
