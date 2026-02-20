{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.BARK where

import Crypto.Hash qualified as Hash
import Data.ByteArray (ByteArrayAccess, convert, eq)
import Data.ByteString as BS (ByteString, readFile)
import Data.List (uncons)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import System.FilePath (takeDirectory, (</>))
import Prelude hiding (words)

import Data.BULK
import Data.BULK.Debug (debug)
import Data.BULK.Utils (failLeft)

newtype BARK = BARK [Entry]
    deriving (Eq, Show)

data Entry = Description {path :: FilePath, hash :: Hash}
    deriving (Eq, Show)

data Hash
    = Shake128 ByteString
    | MD5 ByteString
    deriving (Eq, Show)

verifyManifest :: Context -> FilePath -> IO (Either String ())
verifyManifest ctx file = do
    BARK entries <- decodeFile ctx file >>= failLeft
    Right () <$ (sequenceA <$> traverse (verifyManifestEntry $ takeDirectory file) entries >>= failLeft)

verifyManifestEntry :: FilePath -> Entry -> IO (Either String ())
verifyManifestEntry root entry = do
    content <- BS.readFile $ root </> entry.path
    let (entryDigest, fileDigest) = case entry.hash of
            Shake128 digest -> (digest, toBS $ Hash.hashWith (Hash.SHAKE128 :: Hash.SHAKE128 256) content)
            MD5 digest -> (digest, toBS $ Hash.hashWith Hash.MD5 content)
    pure $
        if entryDigest `eq` fileDigest
            then
                Right ()
            else
                Left [i|expected digest #{debug entryDigest} but got #{debug fileDigest}|]

toBS :: (ByteArrayAccess ba) => ba -> ByteString
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
