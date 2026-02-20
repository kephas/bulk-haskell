module Data.BULK.API where

import Data.BULK.Decode qualified as D
import Data.BULK.Encode qualified as E
import Data.BULK.TextNotation qualified as TN
import Data.BULK.ToFrom qualified as TF
import Data.BULK.Types (BULK, Context, Warning)
import Data.BULK.Utils (runWarningsAndError)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Polysemy (Embed, Sem, run, runM)
import Polysemy.Error (Error)
import Polysemy.Output (Output)

-- Decode

readFile :: FilePath -> IO (Either String BULK)
readFile = runAllIO . D.readFile

readFileV1 :: FilePath -> IO (Either String BULK)
readFileV1 = runAllIO . D.readFileV1

parseStream :: LazyByteString -> Either String BULK
parseStream = runAll . D.parseStream

parseStreamV1 :: LazyByteString -> Either String BULK
parseStreamV1 = runAll . D.parseStreamV1

-- Encode

encode :: [BULK] -> Either String LazyByteString
encode = runAll . E.encode

-- TextNotation

parseNotation :: Text -> Either String LazyByteString
parseNotation = runAll . TN.parseNotation

parseNotationFile :: FilePath -> IO (Either String BULK)
parseNotationFile = runAllIO . TN.parseNotationFile

parseNotationFileBin :: FilePath -> IO (Either String LazyByteString)
parseNotationFileBin = runAllIO . TN.parseNotationFileBin

-- ToFrom

fromBULK :: (TF.FromBULK a) => BULK -> Either String a
fromBULK = runAll . TF.fromBULK

decode :: (TF.FromBULK a) => Context -> LazyByteString -> Either String a
decode ctx = runAll . TF.decode ctx

decodeNotation :: (TF.FromBULK a) => Context -> Text -> Either String a
decodeNotation ctx = runAll . TF.decodeNotation ctx

decodeFile :: (TF.FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeFile ctx = runAllIO . TF.decodeFile ctx

decodeBinaryFile :: (TF.FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeBinaryFile ctx = runAllIO . TF.decodeBinaryFile ctx

decodeNotationFile :: (TF.FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeNotationFile ctx = runAllIO . TF.decodeNotationFile ctx

loadNotationFiles :: Context -> [FilePath] -> IO (Either String Context)
loadNotationFiles ctx = runAllIO . TF.loadNotationFiles ctx

-- interpreters

runAll :: Sem [Error String, Output Warning] a -> Either String a
runAll = run . runWarningsAndError

runAllIO :: Sem [Error String, Output Warning, Embed IO] a -> IO (Either String a)
runAllIO = runM . runWarningsAndError
