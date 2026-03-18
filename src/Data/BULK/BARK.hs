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
import Data.Text qualified as T
import Polysemy.Reader (runReader)
import System.FilePath (takeDirectory, (</>))
import Prelude hiding (words)

import Data.BULK (CheckDigest (..), Context, FromBULK (..), Name (..), Namespace (..), NamespaceID (..), Ref (..), ToBULK (toBULK), Value (..), hex, list, namedRef, nextBULK, string, withFormCase, (.:), (<*:>), (<:>))
import Data.BULK.API (runAllIO)
import Data.BULK.Debug (Debug, debug)
import Data.BULK.From (decodeFile)
import Data.BULK.To (encodeFile)
import Data.BULK.Types (BULK (..))
import Data.BULK.Utils (IOE, readFileBS)

newtype BARK = BARK [Entry]
    deriving (Eq, Show)

data Entry = Description {path :: FilePath, hash :: Hash}
    deriving (Eq, Show)

data Hash
    = Hash {alg :: HashAlg, digest :: StrictByteString}
    deriving (Eq, Show)

data HashAlg = Shake128 | MD5
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
    let entryDigest = entry.hash.digest
        fileDigest = case entry.hash.alg of
            Shake128 -> hashShake128 content
            MD5 -> hashMD5 content
    pure $
        if entryDigest `eq` fileDigest
            then
                OK entry.path
            else
                Failed entry.path [i|expected digest #{debug entryDigest} but got #{debug fileDigest}|]

createManifest :: Context -> FilePath -> [FilePath] -> IO (Either String ())
createManifest ctx manifestPath files = runAllIO $ runReader ctx do
    manifest <- BARK <$> traverse makeManifestEntry files
    encodeFile manifestPath manifest

makeManifestEntry :: FilePath -> IOE r Entry
makeManifestEntry path = do
    let alg = Shake128
    digest <- hashShake128 <$> readFileBS path
    let hash = Hash{..}
    pure Description{..}

displayVerification :: Verification -> String
displayVerification OK{path} = [i|#{path}: ✅|]
displayVerification Failed{path, reason} = [i|#{path}: 🛑 (#{reason})|]

hashShake128, hashMD5 :: (ByteArrayAccess ba) => ba -> StrictByteString
hashShake128 = hashBS (Hash.SHAKE128 :: Hash.SHAKE128 256)
hashMD5 = hashBS Hash.MD5

hashBS :: (ByteArrayAccess ba, Hash.HashAlgorithm alg) => alg -> ba -> StrictByteString
hashBS alg = toBS . Hash.hashWith alg

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
            "hash"
            [ (bark .: "shake128", Hash Shake128 <$> nextBULK)
            , (bark .: "md5", Hash MD5 <$> nextBULK)
            ]

instance ToBULK BARK where
    toBULK (BARK entries) = do
        barkOp <- namedRef "bark" "bark"
        Form . (barkOp :) <$> traverse toBULK entries

instance ToBULK Entry where
    toBULK Description{..} = do
        descOp <- namedRef "bark" "description"
        metadataOp <- namedRef "bark" "metadata"
        aboutOp <- namedRef "bark" "about"
        pathExpr <- form2 <$> namedRef "bark" "path" <*> toBULK (T.pack path)
        hashExpr <- form2 <$> namedRef "bark" "hash" <*> toBULK hash
        pure $ Form [descOp, Form [metadataOp, Form [aboutOp, pathExpr], hashExpr]]

instance ToBULK Hash where
    toBULK Hash{..} =
        form2 <$> toBULK alg <*> toBULK digest

instance ToBULK HashAlg where
    toBULK Shake128 = namedRef "bark" "shake128"
    toBULK MD5 = namedRef "bark" "md5"

form2 :: BULK -> BULK -> BULK
form2 operator operand = Form [operator, operand]

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

instance Debug BARK where
    debug (BARK entries) = [i|(BARK #{debug entries})|]

instance Debug Entry where
    debug Description{path, hash = Hash{alg, digest}} = [i|(#{path}: #{alg}=#{debug digest}|]
