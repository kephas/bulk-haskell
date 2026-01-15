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

import Control.Monad ((>=>))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.List (find)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding.Error (OnDecodeError, lenientDecode)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Word (Word8)
import Polysemy (Sem, run)
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (State, evalState, get, put)

import Data.BULK.Core (encodeInt, pattern Core)
import Data.BULK.Decode (VersionConstraint (SetVersion), getStream, parseLazy)
import Data.BULK.Encode (encodeNat, pattern IntReference, pattern Nat)
import Data.BULK.Eval (eval)
import Data.BULK.TextNotation (parseTextNotation)
import Data.BULK.Types (BULK (..), FullNamespaceDefinition (..), MatchBULK (..), NameDefinition (..), Namespace (AssociatedNamespace))
import Data.Text.Encoding (encodeUtf8)

class FromBULK a where
    parseBULK :: BULK -> Parser a

class ToBULK a where
    toBULK :: a -> BULK

fromBULK :: (FromBULK a) => BULK -> Either String a
fromBULK = fromBULKWith []

fromBULKWith :: (FromBULK a) => [FullNamespaceDefinition] -> BULK -> Either String a
fromBULKWith nss = runParser . parseBULK . eval nss

decode :: (FromBULK a) => [FullNamespaceDefinition] -> ByteString -> Either String a
decode nss = parseLazy (getStream $ SetVersion 1 0) >=> fromBULKWith nss

decodeNotation :: (FromBULK a) => [FullNamespaceDefinition] -> Text -> Either String a
decodeNotation nss = parseTextNotation >=> decode nss

decodeFile :: (FromBULK a) => [FullNamespaceDefinition] -> FilePath -> IO (Either String a)
decodeFile nss path = decode nss <$> B.readFile path

decodeNotationFile :: (FromBULK a) => [FullNamespaceDefinition] -> FilePath -> IO (Either String a)
decodeNotationFile nss = decodeNotationFileWith nss lenientDecode

decodeNotationFileWith :: (FromBULK a) => [FullNamespaceDefinition] -> OnDecodeError -> FilePath -> IO (Either String a)
decodeNotationFileWith nss onError file = do
    bytes <- B.readFile file
    pure $ decodeNotation nss $ LT.toStrict $ LTE.decodeUtf8With onError bytes

withForm :: (FromBULK a) => MatchBULK -> Parser a -> BULK -> Parser a
withForm match parser (Form (op : content))
    | match.match op = put (Just content) >> parser
    | otherwise = fail [i|not the expected operator: (#{op}) (expected (#{expected}))|]
  where
    expected = match.expected
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
notExpected expected value = fail [i|not a #{expected}: (#{value})|]

instance FromBULK Bool where
    parseBULK (Core 1) = pure True
    parseBULK (Core 2) = pure False
    parseBULK bulk = fail [i|not a boolean: #{bulk}|]

instance FromBULK Int where
    parseBULK (Nat n) = pure n
    parseBULK bulk = fail [i|not an integer: #{bulk}|]

instance FromBULK BULK where
    parseBULK = pure

instance (FromBULK a) => FromBULK [a] where
    parseBULK = withSequence list

type Parser a = Sem '[Fail, State (Maybe [BULK])] a

runParser :: Parser a -> Either String a
runParser = run . evalState Nothing . runFail

rawName :: Int -> Word8 -> MatchBULK
rawName marker name = MatchBULK{..}
  where
    ref = IntReference marker name
    match = (== ref)
    expected = [i|#{ref}|]

nsName :: FullNamespaceDefinition -> Text -> MatchBULK
nsName ns1@(FullNamespaceDefinition{..}) mnemonic1 = MatchBULK{..}
  where
    match :: BULK -> Bool
    match (Reference (AssociatedNamespace _ ns2@(FullNamespaceDefinition{})) name) = maybe False (matchDef ns2 name) foundNameDef
    match _ = False
    foundNameDef :: Maybe NameDefinition
    foundNameDef = find (\name -> name.mnemonic == mnemonic1) names
    matchDef :: FullNamespaceDefinition -> Word8 -> NameDefinition -> Bool
    matchDef ns2 name def = ns1 == ns2 && name == def.marker
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
