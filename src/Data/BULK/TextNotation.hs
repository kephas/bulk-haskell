{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BULK.TextNotation where

import Control.Applicative.Combinators (between)
import Control.Monad.State (MonadState (..), State, evalState, gets, modify, runState)
import Data.Bifunctor (first)
import Data.Bits ((.|.))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Functor (void, ($>))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (..))
import Data.Set (elems)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Word (Word8)
import Text.Megaparsec (ErrorFancy (ErrorFail), MonadParsec (..), ParseError (FancyError), ParseErrorBundle (..), ParsecT, ShowErrorComponent (..), TraversableStream, VisualStream, choice, chunk, errorBundlePretty, noneOf, optional, runParserT, single, some, (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (from)

import Data.BULK.Decode (parseStream)
import Data.BULK.Encode (encodeExpr, encodeNat)
import Data.BULK.Types (BULK (..), Name (..), NamespaceID (CoreNS))

data NotationNS = NotationNS {namespace :: NamespaceID, usedNames :: Map Text Word8, availableNames :: [Word8]}

data NamespaceMap = NamespaceMap {usedNamespaces :: Map Text NotationNS, nextMarker :: Int}

type Parser = ParsecT String Text (State NamespaceMap)

parseNotation :: Text -> Either String ByteString
parseNotation =
    parseNotationNamed ""

parseNotationNamed :: String -> Text -> Either String ByteString
parseNotationNamed name source =
    BB.toLazyByteString <$> runParser showFail name notationP source

parseNotationFile :: FilePath -> IO (Either String BULK)
parseNotationFile = parseNotationFileInto parseStream

parseNotationFileBin :: FilePath -> IO (Either String ByteString)
parseNotationFileBin = parseNotationFileInto Right

parseNotationFileInto :: (ByteString -> Either String a) -> FilePath -> IO (Either String a)
parseNotationFileInto convert file = do
    text <- readTextFile file
    pure $ parseNotationNamed file text >>= convert

readTextFile :: FilePath -> IO Text
readTextFile file = do
    bytes <- B.readFile file
    pure $ LT.toStrict $ LTE.decodeUtf8With lenientDecode bytes

notationP :: Parser BB.Builder
notationP =
    mconcat <$> some (lexeme tokenP)

lexeme :: Parser a -> Parser a
lexeme = (<* (space1 <|> eof))

bulkCoreNames :: (Integral a) => [(Text, a)]
bulkCoreNames = zip (T.words "version import namespace package define mnemonic explain true false unsigned-int signed-int verifiable-ns mnemonic/def") ([0x0 .. 0x6] ++ [0xE, 0xF, 0x13, 0x14, 0xE0, 0xE1])

bulkProfile :: NamespaceMap
bulkProfile = NamespaceMap{usedNamespaces = M.singleton "bulk" NotationNS{namespace = CoreNS, usedNames = coreNames, availableNames = []}, nextMarker = 0x14}
  where
    coreNames = M.fromList bulkCoreNames

w8 :: (Applicative f) => Word8 -> f BB.Builder
w8 = pure . BB.word8

tokenP :: Parser BB.Builder
tokenP = label "token" $ nestedP <|> stringP <|> symbolP "nil" 0 <|> symbolP "(" 1 <|> symbolP ")" 2 <|> smallIntP <|> smallArrayP <|> symbolP "#" 3 <|> literalBytesP <|> stringP <|> coreP <|> try referenceP <|> decimalP

symbolP :: Text -> Word8 -> Parser BB.Builder
symbolP sym word =
    chunk sym *> w8 word

nestedP :: Parser BB.Builder
nestedP = do
    label "nested BULK" $ between open close $ encodeExpr . Array . BB.toLazyByteString <$> notationP
  where
    open = lexeme $ chunk "(["
    close = chunk "])"

smallIntP :: Parser BB.Builder
smallIntP = label "small int" do
    value <- chunk "w6[" *> L.decimal <* chunk "]"
    if value < 64
        then
            w8 $ 0x80 .|. value
        else
            fail $ "too big for a small unsigned integer: " <> show value

smallArrayP :: Parser BB.Builder
smallArrayP = label "small array" do
    size <- chunk "#[" *> L.decimal <* chunk "]"
    w8 $ 0xC0 .|. size

literalBytesP :: Parser BB.Builder
literalBytesP = do
    void $ chunk "0x"
    someBytes
  where
    someBytes :: Parser BB.Builder
    someBytes = do
        void $ optional (single '-')
        byte <- byteP
        rest <- try someBytes <|> pure mempty
        pure $ BB.word8 byte <> rest

decimalP :: Parser BB.Builder
decimalP =
    encodeExpr . encodeNat <$> (L.decimal :: Parser Int)

quoteP, notQuoteP :: Parser Text
quoteP = T.singleton <$> single '"'
notQuoteP = takeWhileP (Just "not quotes") ('"' /=)

stringP :: Parser BB.Builder
stringP =
    encodeExpr . Array . LTE.encodeUtf8 . LT.fromStrict <$> between quoteP quoteP notQuoteP

referenceP :: Parser BB.Builder
referenceP = qualified <|> unqualified
  where
    qualified = do
        ns <- mnemonicP
        void $ single ':'
        name <- mnemonicP
        ref <- ensureRef ns name
        pure $ encodeExpr ref
    unqualified = do
        name <- mnemonicP
        ref <- ensureRef "<empty>" name
        pure $ encodeExpr ref

mnemonicP :: Parser Text
mnemonicP = T.pack <$> some (noneOf @[] " :")

ensureRef :: Text -> Text -> Parser BULK
ensureRef nsMnemonic nameMnemonic = do
    ns <- ensureNamespace
    name <- ensureName ns
    pure $ Reference{name = Name ns.namespace name, mnemonic = Just nameMnemonic}
  where
    ensureNamespace :: Parser NotationNS
    ensureNamespace = do
        mNs <- gets lookupNS
        maybe createNamespace pure mNs
    ensureName :: NotationNS -> Parser Word8
    ensureName ns = maybe (createName ns) pure $ M.lookup nameMnemonic ns.usedNames
    lookupNS :: NamespaceMap -> Maybe NotationNS
    lookupNS nss = M.lookup nsMnemonic nss.usedNamespaces
    createNamespace = do
        nss <- get
        let marker = nss.nextMarker
            newNamespace = NotationNS{namespace = from marker, usedNames = M.empty, availableNames = [0 .. 255]}
        put nss{usedNamespaces = M.insert nsMnemonic newNamespace nss.usedNamespaces, nextMarker = succ marker}
        pure newNamespace
    createName :: NotationNS -> Parser Word8
    createName ns = case ns.availableNames of
        (nextName : otherNames) -> do
            let ns' = ns{usedNames = M.insert nameMnemonic nextName ns.usedNames, availableNames = otherNames}
            modify $ \nss -> nss{usedNamespaces = M.insert nsMnemonic ns' nss.usedNamespaces}
            pure nextName
        [] -> fail [i|no space available for #{nsMnemonic}:#{nameMnemonic}|]

coreP :: Parser BB.Builder
coreP = optional (chunk "bulk:") >> choice parsers
  where
    -- they need to be sorted in reverse to try the longer ones before their prefix (e.g. try foo* before foo)
    parsers = map mkParser $ sortOn (Down . fst) bulkCoreNames
    mkParser (mnemonic, num) = try $ chunk mnemonic $> (BB.word8 0x10 <> BB.word8 num)

instance ShowErrorComponent [Char] where
    showErrorComponent = id

showFail :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
showFail (ParseErrorBundle{bundleErrors = (FancyError _ errs) :| _})
    | [ErrorFail err] <- elems errs = err
showFail bundle = errorBundlePretty bundle

byteP :: Parser Word8
byteP = do
    high <- halfByteP highHalfBytes
    low <- halfByteP lowHalfBytes
    pure $ high .|. low

halfByteP :: [Word8] -> Parser Word8
halfByteP values = choice $ zipWith mkCharParser hexChars values
  where
    mkCharParser :: Char -> Word8 -> Parser Word8
    mkCharParser char value = single char $> value

hexChars :: [Char]
hexChars = "0123456789abcdefABCDEF"

lowHalfBytes, highHalfBytes :: [Word8]
lowHalfBytes = [0x0 .. 0xF] ++ [0xA .. 0xF]
highHalfBytes = [0x00, 0x10 .. 0xF0] ++ [0xA0, 0xB0 .. 0xF0]

testP :: Parser Int
testP = choice (zipWith mkParser ["foo-bar", "foo"] [1, 2]) <* eof
  where
    mkParser text int = try $ chunk text $> int

runParser :: (ParseErrorBundle Text String -> String) -> String -> Parser a -> Text -> Either String a
runParser mapError name parser =
    first mapError . flip evalState bulkProfile . runParserT parser name

runParserM :: (MonadState NamespaceMap m) => (ParseErrorBundle Text String -> String) -> Parser a -> Text -> m (Either String a)
runParserM mapError parser text = do
    result <- state $ runState $ runParserT parser "-" text
    pure $ first mapError result

debugParse :: (Show a) => Parser a -> Text -> IO ()
debugParse parser text = do
    putStrLn $ either id show (runParser errorBundlePretty "debug" parser text)
