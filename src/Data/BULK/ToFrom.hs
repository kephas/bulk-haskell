{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.ToFrom where

import Control.Monad ((<=<), (>=>))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.List (find)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Error (OnDecodeError, lenientDecode)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Polysemy (Sem, run)
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (State, evalState, get, put)

import Data.BULK.Core (encodeInt)
import Data.BULK.Decode (VersionConstraint (ReadVersion), getStream, parseLazy)
import Data.BULK.Encode (encodeNat, pattern Nat)
import Data.BULK.Eval (eval)
import Data.BULK.TextNotation (parseTextNotation)
import Data.BULK.Types (BULK (..), MatchBULK (..), Name (..), NameDefinition (..), Namespace (AssociatedNamespace), NamespaceDefinition (..), pattern Core)

class FromBULK a where
    parseBULK :: BULK -> Parser a

class ToBULK a where
    toBULK :: a -> BULK

fromBULK :: (FromBULK a) => BULK -> Either String a
fromBULK = fromBULKWith []

fromBULKWith :: (FromBULK a) => [NamespaceDefinition] -> BULK -> Either String a
fromBULKWith nss = runParser . parseBULK <=< eval nss

decode :: (FromBULK a) => [NamespaceDefinition] -> ByteString -> Either String a
decode nss = parseLazy (getStream ReadVersion) >=> fromBULKWith nss

decodeNotation :: (FromBULK a) => [NamespaceDefinition] -> Text -> Either String a
decodeNotation nss = parseTextNotation >=> decode nss

decodeFile :: (FromBULK a) => [NamespaceDefinition] -> FilePath -> IO (Either String a)
decodeFile nss path = decode nss <$> B.readFile path

decodeNotationFile :: (FromBULK a) => [NamespaceDefinition] -> FilePath -> IO (Either String a)
decodeNotationFile nss = decodeNotationFileWith nss lenientDecode

decodeNotationFileWith :: (FromBULK a) => [NamespaceDefinition] -> OnDecodeError -> FilePath -> IO (Either String a)
decodeNotationFileWith nss onError file = do
    bytes <- B.readFile file
    pure $ decodeNotation nss $ LT.toStrict $ LTE.decodeUtf8With onError bytes

withForm :: (FromBULK a) => MatchBULK -> Parser a -> BULK -> Parser a
withForm MatchBULK{..} parser (Form (op : content))
    | match op = put (Just content) >> parser
    | otherwise = fail [i|not the expected operator: (#{op}) (expected (#{expected}))|]
withForm _ref _parser bulk = notExpected "form" bulk

withStream :: (FromBULK a) => Parser a -> BULK -> Parser a
withStream = withSequence

withSequence :: (FromBULK a) => Parser a -> BULK -> Parser a
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

notExpected :: (MonadFail m) => String -> BULK -> m a
notExpected expected value = fail [i|cannot parse as #{expected}: #{value}|]

instance FromBULK () where
    parseBULK Nil = pure ()
    parseBULK bulk = notExpected "nil" bulk

instance FromBULK Bool where
    parseBULK (Core 1) = pure True
    parseBULK (Core 2) = pure False
    parseBULK bulk = notExpected "boolean" bulk

instance FromBULK Int where
    parseBULK (Nat n) = pure n
    parseBULK bulk = notExpected "integer" bulk

instance FromBULK BULK where
    parseBULK = pure

instance (FromBULK a) => FromBULK [a] where
    parseBULK = withSequence list

type Parser a = Sem '[Fail, State (Maybe [BULK])] a

runParser :: Parser a -> Either String a
runParser = run . evalState Nothing . runFail

nsName :: NamespaceDefinition -> Text -> MatchBULK
nsName ns1@(NamespaceDefinition{..}) mnemonic1 =
    MatchBULK{..}
  where
    match = maybe (const False) matchDef $ find (\name -> name.mnemonic == mnemonic1) names
    matchDef def (Reference (Name (AssociatedNamespace ns2@(NamespaceDefinition{})) name)) = ns1 == ns2 && name == def.marker
    matchDef _ _ = False
    expected = [i|#{mnemonic}:#{mnemonic1}|]

instance ToBULK Bool where
    toBULK True = Core 1
    toBULK False = Core 2

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
