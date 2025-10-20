{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Data.BULK.ToFrom where

import Data.ByteString.Lazy (ByteString)
import Data.String.Interpolate (i)
import Polysemy (Sem, run)
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (State, evalState, get, put)

import Control.Monad ((>=>))
import Data.BULK.Core (pattern Core)
import Data.BULK.Decode (VersionConstraint (SetVersion), getStream, parseLazy)
import Data.BULK.Encode (pattern Nat)
import Data.BULK.Eval (eval)
import Data.BULK.TextNotation (parseTextNotation)
import Data.BULK.Types (BULK (..))
import Data.Text (Text)
import Data.Word (Word8)

class FromBULK a where
    parseBULK :: BULK -> Parser a

fromBULK :: (FromBULK a) => BULK -> Either String a
fromBULK = runParser . parseBULK . eval

decode :: (FromBULK a) => ByteString -> Either String a
decode = parseLazy (getStream $ SetVersion 1 0) >=> fromBULK

decodeNotation :: (FromBULK a) => Text -> Either String a
decodeNotation = parseTextNotation >=> decode

withForm :: (FromBULK a) => BULK -> Parser a -> BULK -> Parser a
withForm ref parser (Form (op : content))
    | ref == op = put (Just content) >> parser
    | otherwise = fail [i|not the expected operator: (#{op}) (expected (#{ref}))|]
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
