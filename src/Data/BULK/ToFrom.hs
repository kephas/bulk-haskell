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

import Data.ByteString.Lazy (ByteString)
import Data.String.Interpolate (i)
import Polysemy (Sem, run)
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (State, evalState, get, put)

import Control.Monad ((>=>))
import Data.BULK.Core (pattern Core)
import Data.BULK.Decode (VersionConstraint (SetVersion), getStream, parseLazy)
import Data.BULK.Encode (pattern IntReference, pattern Nat)
import Data.BULK.Eval (eval)
import Data.BULK.TextNotation (parseTextNotation)
import Data.BULK.Types (BULK (..), FullNamespaceDefinition (..), MatchBULK (..), NameDefinition (..), Namespace (AssociatedNamespace))
import Data.List (find)
import Data.Text (Text)
import Data.Word (Word8)

class FromBULK a where
    parseBULK :: BULK -> Parser a

fromBULK :: (FromBULK a) => BULK -> Either String a
fromBULK = fromBULKWith []

fromBULKWith :: (FromBULK a) => [FullNamespaceDefinition] -> BULK -> Either String a
fromBULKWith nss = runParser . parseBULK . eval nss

decode :: (FromBULK a) => [FullNamespaceDefinition] -> ByteString -> Either String a
decode nss = parseLazy (getStream $ SetVersion 1 0) >=> fromBULKWith nss

decodeNotation :: (FromBULK a) => [FullNamespaceDefinition] -> Text -> Either String a
decodeNotation nss = parseTextNotation >=> decode nss

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
