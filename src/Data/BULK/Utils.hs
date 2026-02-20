{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Data.BULK.Utils where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Hex (unhexM)
import Data.Map.Strict qualified as M
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (stringE)
import Language.Haskell.TH.QuasiQuoter (QuasiQuoter (quoteExp), namedDefaultQuasiQuoter)
import Polysemy (Embed, InterpreterFor, Member, Members, Sem)
import Polysemy.Error (Error, catch, runError, throw)
import Polysemy.Output (Output, output, runOutputList)
import Polysemy.State (State, evalState, get)
import System.IO (IOMode (ReadMode), withFile)

import Control.Exception (SomeException, displayException, try)
import Data.BULK.Types (Namespace (..), NamespaceID, Representation (..), Result (..), Warning (..))
import Data.ByteString (StrictByteString)
import Polysemy.Embed (embed)
import Polysemy.Fail (Fail)

hex :: QuasiQuoter
hex = (namedDefaultQuasiQuoter "hex"){quoteExp = unhexM >=> stringE}

insertIfMissing :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insertIfMissing = M.insertWith (\_new old -> old)

bareNS :: NamespaceID -> Namespace
bareNS nsId = Namespace{matchID = nsId, mnemonic = "", names = []}

runWarningsAndError :: Sem (Error String : Output Warning : r) a -> Sem r (Either String a)
runWarningsAndError action = do
    (warnings, result) <- runOutputList $ runError action
    pure $ first (appendWarnings warnings) result

appendWarnings :: [Warning] -> String -> String
appendWarnings [] str =
    str
appendWarnings warnings str =
    str <> "\n" <> unlines (map (.unWarning) warnings)

warn :: (Member (Output Warning) r) => String -> Sem r ()
warn = output . Warning

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

type IOE r a = (Members [Embed IO, Error String, Output Warning] r) => Sem r a

readFileLBS :: FilePath -> IOE r LazyByteString
readFileLBS path =
    tryIO $ BL.readFile path

readFileBS :: FilePath -> IOE r StrictByteString
readFileBS path =
    tryIO $ BS.readFile path

tryIO :: IO a -> IOE r a
tryIO action = embed (try @SomeException action) >>= either (throw . displayException) pure

runLocalState :: (Member (State s) r) => Sem (State s : r) a -> Sem r (s, a)
runLocalState action = evalLocalState do
    result <- action
    state_ <- get
    pure (state_, result)

evalLocalState :: (Member (State s) r) => Sem (State s : r) a -> Sem r a
evalLocalState action = do
    state_ <- get
    evalState state_ action

errorToFail :: (Member Fail r) => InterpreterFor (Error String) r
errorToFail action =
    runError action >>= failLeft

leftIn :: String -> Either String a -> Either String a
leftIn place = first \err -> [i|#{place}: #{err}|]

failLeftIn :: (HasCallStack) => FilePath -> Either String a -> IO a
failLeftIn file = failLeft . leftIn file

failLeft :: (HasCallStack, MonadFail m) => Either String a -> m a
failLeft = either fail pure

probeRepr :: FilePath -> IOE r Representation
probeRepr path = do
    start <- embed $ readStart 4 path
    pure if start == "\x01\x10\x00\x81" then BinaryBULK else TextBULK

readStart :: Int -> FilePath -> IO BS.ByteString
readStart size file =
    withFile file ReadMode (`BS.hGet` size)

represent :: a -> a -> Representation -> a
represent onBin _onText BinaryBULK = onBin
represent _onBin onText TextBULK = onText

placeError :: (Member (Error String) r) => String -> Sem r a -> Sem r a
placeError place action = do
    catch action (throw . addPlace)
  where
    addPlace err = [i|#{place}: #{err}|]

errIn :: String -> Result a -> Result a
errIn place = errMap \err -> [i|#{place}: #{err}|]

errMap :: (String -> String) -> Result a -> Result a
errMap f (Result res) = Result $ first f res

liftFailIn :: (HasCallStack) => FilePath -> Result a -> IO a
liftFailIn file = liftFail . errIn file

liftFail :: (HasCallStack) => Result a -> IO a
liftFail = either fail pure . (.unResult)
