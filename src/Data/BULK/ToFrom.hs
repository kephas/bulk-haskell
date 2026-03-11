{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Data.BULK.ToFrom where

import Control.Monad (foldM, (>=>))
import Data.ByteString (StrictByteString, toStrict)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.List (find)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Polysemy (Member, Members, Sem, raise, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Fail (Fail, runFail)
import Polysemy.Output (Output)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State (State, evalState, get, put)

import Data.BULK.Core (encodeInt)
import Data.BULK.Core qualified as Core
import Data.BULK.Debug (debug)
import Data.BULK.Decode (parseStream)
import Data.BULK.Encode (encodeNat, pattern Nat)
import Data.BULK.Eval (eval, execContext, mkContext, parseText)
import Data.BULK.TextNotation (parseNotation, parseNotationFile)
import Data.BULK.Types (BULK (..), Context (..), MatchBULK (..), Name (..), Namespace (..), Ref (..), Warning)
import Data.BULK.Utils (IOE, errorToFail, liftMaybe, placeError, probeRepr, readFileLBS, represent)

class FromBULK a where
    parseBULK :: BULK -> Parser a

class ToBULK a where
    encodeBULK :: a -> Encoder BULK

fromBULK :: (Members [Error String, Output Warning] r, FromBULK a) => BULK -> Sem r a
fromBULK = fromBULKWith $ mkContext []

fromBULKWith :: (Members [Error String, Output Warning] r, FromBULK a) => Context -> BULK -> Sem r a
fromBULKWith ctx bulk = do
    evaled <- eval ctx bulk
    runParser $ parseBULK evaled

toBULK :: (Members [Error String, Output Warning, Reader Context] r, ToBULK a) => a -> Sem r BULK
toBULK value = do
    ctx <- ask
    runEncoder ctx $ encodeBULK value

toBULKWith :: (Members [Error String, Output Warning] r, ToBULK a) => Context -> a -> Sem r BULK
toBULKWith ctx value =
    runReader ctx $ toBULK value

decode :: (FromBULK a) => (Members [Error String, Output Warning] r) => Context -> ByteString -> Sem r a
decode ctx = parseStream >=> fromBULKWith ctx

decodeNotation :: (FromBULK a) => (Members [Error String, Output Warning] r) => Context -> Text -> Sem r a
decodeNotation ctx = parseNotation >=> decode ctx

decodeFile :: (FromBULK a) => Context -> FilePath -> IOE r a
decodeFile ctx path = do
    decoder <- represent decodeBinaryFile decodeNotationFile <$> (probeRepr path)
    decoder ctx path

decodeBinaryFile :: (FromBULK a) => Context -> FilePath -> IOE r a
decodeBinaryFile ctx path = placeError path $ decode ctx =<< readFileLBS path

decodeNotationFile :: (FromBULK a) => Context -> FilePath -> IOE r a
decodeNotationFile ctx file = do
    bulk <- parseNotationFile file
    placeError file $ fromBULKWith ctx bulk

loadNotationFiles :: Context -> [FilePath] -> IOE r Context
loadNotationFiles = foldM loadNotationFile
  where
    loadNotationFile ctx file = do
        bulk <- parseNotationFile file
        placeError file $ execContext ctx bulk

(<*:>) :: Namespace -> Text -> Parser a -> BULK -> Parser a
ns <*:> name = withForm (ns .: name)

(<:>) :: Namespace -> Text -> Parser a -> Parser a
ns <:> name = withNext . withForm (ns .: name)

withForm :: MatchBULK -> Parser a -> BULK -> Parser a
withForm MatchBULK{..} parser (Form (op : content))
    | match op = raise $ evalState (Just content) parser
    | otherwise = fail [i|not the expected operator: (#{debug op}) (expected (#{expected}))|]
withForm _ref _parser bulk = notExpected "form" bulk

withFormCase :: [(MatchBULK, Parser a)] -> BULK -> Parser a
withFormCase [] _bulk = fail "nothing matched"
withFormCase ((matcher, parser) : rest) bulk@(Form (op : content)) =
    if matcher.match op
        then raise $ evalState (Just content) parser
        else withFormCase rest bulk
withFormCase _matchers bulk = notExpected "form" bulk

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
parseString bulk =
    errorToFail $ unpack <$> parseText bulk

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

type Parser a = Sem '[State (Maybe [BULK]), Fail] a

runParser :: (Member (Error String) r) => Parser a -> Sem r a
runParser = either throw pure . run . runFail . evalState Nothing

(.:) :: Namespace -> Text -> MatchBULK
(.:) ns1@(Namespace{mnemonic}) mnemonic1 =
    MatchBULK{..}
  where
    match (Reference (Ref ns2 Name{mnemonic = Just mnemonic2})) = ns1.matchID == ns2 && mnemonic1 == mnemonic2
    match _bulk = False
    expected = [i|#{mnemonic}:#{mnemonic1}|]

askName :: (Members [Error String, Reader Context] r) => Text -> Text -> Sem r BULK
askName nsMnemonic nameMnemonic = do
    ctx <- ask
    ns <- liftMaybe [i|no namespace #{nsMnemonic}|] $ S.lookupMin $ S.filter ((== nsMnemonic) . (.mnemonic)) $ ctx.namespaces
    name <- liftMaybe [i|no name #{ns}:#{nameMnemonic}|] $ find ((== (Just nameMnemonic)) . (.mnemonic)) ns.names
    pure $ Reference $ Ref ns.matchID name

instance ToBULK Bool where
    encodeBULK True = pure Core.True
    encodeBULK False = pure Core.False

instance ToBULK Int where
    encodeBULK num
        | num >= 0 = pure $ encodeNat num
        | otherwise = pure $ encodeInt num

instance (ToBULK a) => ToBULK [a] where
    encodeBULK xs = Form <$> traverse encodeBULK xs

instance ToBULK Text where
    encodeBULK = encodeBULK . B.fromStrict . encodeUtf8

instance ToBULK ByteString where
    encodeBULK = pure . Array

type Encoder a = Sem '[Reader Context, Error String] a

runEncoder :: (Member (Error String) r) => Context -> Encoder a -> Sem r a
runEncoder ctx = either throw pure . run . runError . runReader ctx
