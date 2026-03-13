module Data.BULK.API where

import Data.BULK.Decode qualified as D
import Data.BULK.Encode qualified as E
import Data.BULK.Eval (mkContext)
import Data.BULK.From qualified as From
import Data.BULK.TextNotation qualified as TN
import Data.BULK.To qualified as To
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

-- From

fromBULK :: (From.FromBULK a) => BULK -> Either String a
fromBULK = runAll . From.fromBULK

toBULK :: (To.ToBULK a) => a -> Either String BULK
toBULK = runAll . To.toBULKWith (mkContext [])

decode :: (From.FromBULK a) => Context -> LazyByteString -> Either String a
decode ctx = runAll . From.decode ctx

decodeNotation :: (From.FromBULK a) => Context -> Text -> Either String a
decodeNotation ctx = runAll . From.decodeNotation ctx

decodeFile :: (From.FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeFile ctx = runAllIO . From.decodeFile ctx

decodeBinaryFile :: (From.FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeBinaryFile ctx = runAllIO . From.decodeBinaryFile ctx

decodeNotationFile :: (From.FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeNotationFile ctx = runAllIO . From.decodeNotationFile ctx

loadNotationFiles :: Context -> [FilePath] -> IO (Either String Context)
loadNotationFiles ctx = runAllIO . From.loadNotationFiles ctx

-- interpreters

runAll :: Sem [Error String, Output Warning] a -> Either String a
runAll = run . runWarningsAndError

runAllIO :: Sem [Error String, Output Warning, Embed IO] a -> IO (Either String a)
runAllIO = runM . runWarningsAndError
