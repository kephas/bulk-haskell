{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.ToFrom where

import Control.Monad (foldM, (>=>))
import Data.ByteString (StrictByteString, toStrict)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack (HasCallStack)
import Polysemy (Sem, raise, run)
import Polysemy.Error (Error)
import Polysemy.Fail (Fail, failToError)
import Polysemy.Output (Output)
import Polysemy.State (State, evalState, get, put)
import Witch (from)

import Data.BULK.Core (encodeInt)
import Data.BULK.Core qualified as Core
import Data.BULK.Debug (debug)
import Data.BULK.Decode (parseStream)
import Data.BULK.Encode (encodeNat, pattern Nat)
import Data.BULK.Eval (emptyScope, evalCtx, evalExpr, execContext, mkContext, parseText)
import Data.BULK.TextNotation (parseNotation, parseNotationFile)
import Data.BULK.Types (BULK (..), Context (..), MatchBULK (..), Name (..), Namespace (..), Ref (..), Scope, Warning)
import Data.BULK.Utils (failLeftIn, leftIn, runWarningsAndError)

class FromBULK a where
    parseBULK :: BULK -> Parser a

class ToBULK a where
    toBULK :: a -> BULK

fromBULK :: (FromBULK a) => BULK -> Either String a
fromBULK = fromBULKWith $ mkContext []

fromBULKWith :: (FromBULK a) => Context -> BULK -> Either String a
fromBULKWith ctx bulk = runParser $ evalCtx ctx bulk >>= parseBULK

decode :: (FromBULK a) => Context -> ByteString -> Either String a
decode ctx = parseStream >=> fromBULKWith ctx

decodeNotation :: (FromBULK a) => Context -> Text -> Either String a
decodeNotation ctx = parseNotation >=> decode ctx

decodeFile :: (HasCallStack, FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeFile ctx path = leftIn path . decode ctx <$> B.readFile path

decodeNotationFile :: (HasCallStack, FromBULK a) => Context -> FilePath -> IO (Either String a)
decodeNotationFile ctx file = do
    bulk <- parseNotationFile file >>= either fail pure
    pure $ leftIn file $ fromBULKWith ctx bulk

loadNotationFiles :: (HasCallStack) => Context -> [FilePath] -> IO Context
loadNotationFiles = foldM loadNotationFile
  where
    loadNotationFile ctx file = do
        bulk <- parseNotationFile file >>= failLeftIn file
        failLeftIn file $ execContext $ put (from ctx) >> evalExpr bulk

(<*:>) :: Namespace -> Text -> Parser a -> BULK -> Parser a
ns <*:> name = withForm (nsName ns name)

(<:>) :: Namespace -> Text -> Parser a -> Parser a
ns <:> name = withNext . withForm (nsName ns name)

withForm :: MatchBULK -> Parser a -> BULK -> Parser a
withForm MatchBULK{..} parser (Form (op : content))
    | match op = raise $ evalState (Just content) parser
    | otherwise = fail [i|not the expected operator: (#{debug op}) (expected (#{expected}))|]
withForm _ref _parser bulk = notExpected "form" bulk

withNext :: (BULK -> Parser a) -> Parser a
withNext = (nextBULK >>=)

withStream :: Parser a -> BULK -> Parser a
withStream = withSequence

withSequence :: Parser a -> BULK -> Parser a
withSequence parser (Form content) = put (Just content) >> parser
withSequence _parser bulk = notExpected "form" bulk

nextBULK :: (FromBULK a) => Parser a
nextBULK = do
    context <- get
    case context of
        Nothing -> fail "cannot get the next BULK expression outside of a form"
        Just [] -> fail "no next BULK expression"
        Just (x : xs) -> do
            put $ Just xs
            parseBULK x

list :: (FromBULK a) => Parser [a]
list = do
    context <- get
    case context of
        Nothing -> fail "cannot get a list of BULK expressions outside of a form"
        Just xs -> do
            put $ Just []
            traverse parseBULK xs

parseString :: BULK -> Parser String
parseString bulk = unpack <$> parseText bulk

string :: Parser String
string = withNext parseString

notExpected :: (MonadFail m) => String -> BULK -> m a
notExpected expected value = fail [i|cannot parse as #{expected}: #{debug value}|]

instance FromBULK () where
    parseBULK Nil = pure ()
    parseBULK bulk = notExpected "nil" bulk

instance FromBULK Bool where
    parseBULK Core.True = pure True
    parseBULK Core.False = pure False
    parseBULK bulk = notExpected "boolean" bulk

instance FromBULK Int where
    parseBULK (Nat n) = pure n
    parseBULK bulk = notExpected "integer" bulk

instance FromBULK StrictByteString where
    parseBULK (Array lbs) = pure $ toStrict lbs
    parseBULK bulk = notExpected "array" bulk

instance FromBULK BULK where
    parseBULK = pure

instance (FromBULK a) => FromBULK [a] where
    parseBULK = withSequence list

type Parser a = Sem '[State (Maybe [BULK]), State Scope, Fail, Error String, Output Warning] a

runParser :: Parser a -> Either String a
runParser = run . runWarningsAndError . failToError id . evalState emptyScope . evalState Nothing

nsName :: Namespace -> Text -> MatchBULK
nsName ns1@(Namespace{mnemonic}) mnemonic1 =
    MatchBULK{..}
  where
    match (Reference (Ref ns2 Name{mnemonic = Just mnemonic2})) = ns1.matchID == ns2 && mnemonic1 == mnemonic2
    match _bulk = False
    expected = [i|#{mnemonic}:#{mnemonic1}|]

instance ToBULK Bool where
    toBULK True = Core.True
    toBULK False = Core.False

instance ToBULK Int where
    toBULK num
        | num >= 0 = encodeNat num
        | otherwise = encodeInt num

instance (ToBULK a) => ToBULK [a] where
    toBULK = Form . map toBULK

instance ToBULK Text where
    toBULK = Array . B.fromStrict . encodeUtf8

instance ToBULK ByteString where
    toBULK = Array
