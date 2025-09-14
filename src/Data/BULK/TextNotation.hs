{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.TextNotation where

import Control.Monad.State (MonadState (..), State, evalState, gets, modify, runState)
import Data.Bifunctor (first)
import Data.Bits ((.|.))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (sortOn)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (..))
import Data.Set (elems)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding.Error (OnDecodeError, lenientDecode)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ErrorFancy (ErrorFail), MonadParsec (..), ParseError (FancyError), ParseErrorBundle (..), Parsec, ParsecT, ShowErrorComponent (..), TraversableStream, VisualStream, anySingleBut, choice, chunk, errorBundlePretty, many, match, noneOf, oneOf, optional, parseMaybe, runParserT, satisfy, single, some, takeRest, (<|>))
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer qualified as L

import Data.BULK.Decode (BULK (..), VersionConstraint (ReadVersion), getStream, parseLazy)
import Data.BULK.Encode (encodeExpr, encodeInt)
import Data.Maybe (fromMaybe)

data Namespace = Namespace {marker :: Int, usedNames :: Map Text Int, availableNames :: [Int]}

data NamespaceMap = NamespaceMap {usedNamespaces :: Map Text Namespace, nextMarker :: Int}

type Parser = ParsecT String Text (State NamespaceMap)

parseTextNotation :: Text -> Either String ByteString
parseTextNotation source = do
    lexemes <- runParser showFail lexerP source
    builders <- flip evalState bulkProfile $ sequence <$> traverse parseTextToken lexemes
    pure $ BB.toLazyByteString $ mconcat builders

parseTextFile :: FilePath -> IO (Either String BULK)
parseTextFile = parseTextFileWith lenientDecode ReadVersion

parseTextFileWith :: OnDecodeError -> VersionConstraint -> FilePath -> IO (Either String BULK)
parseTextFileWith onerror constraint file = do
    bytes <- B.readFile file
    pure $ parseTextNotation (LT.toStrict $ LTE.decodeUtf8With onerror bytes) >>= parseLazy (getStream constraint)

bulkCoreNames :: (Integral a) => [(Text, a)]
bulkCoreNames = zip (T.words "version true false ns package import define mnemonic/def ns-mnemonic verifiable-ns concat subst arg rest stringenc iana-charset codepage string string* blob nested-bulk unsigned-int signed-int fraction binary-float decimal-float binary-fixed decimal-fixed decimal2 prefix prefix* postfix postfix* arity") ([0x0 .. 0xD] ++ [0x10 .. 0x16] ++ [0x20 .. 0x27] ++ [0x30 .. 0x34])

bulkProfile :: NamespaceMap
bulkProfile = NamespaceMap{usedNamespaces = M.singleton "bulk" Namespace{marker = 0x10, usedNames = coreNames, availableNames = []}, nextMarker = 0x18}
  where
    coreNames = M.fromList bulkCoreNames

lexerP :: Parser [Text]
lexerP =
    try atLeastOne <|> pure []
  where
    atLeastOne :: Parser [Text]
    atLeastOne = do
        lexeme <- (quotedStringP <|> tokenSyntaxP) <* space
        (lexeme :) <$> lexerP

tokenSyntaxP :: Parser Text
tokenSyntaxP = fmap fst $ match $ some $ satisfy $ not . isSpace

parseTextToken :: Text -> State NamespaceMap (Either String BB.Builder)
parseTextToken "nil" = pure $ w8 0
parseTextToken "(" = pure $ w8 1
parseTextToken ")" = pure $ w8 2
parseTextToken token = runParserM showFail tokenP token

w8 :: (Applicative f) => Word8 -> f BB.Builder
w8 = pure . BB.word8

tokenP :: Parser BB.Builder
tokenP = (smallIntP <|> smallArrayP <|> literalBytesP <|> decimalP <|> stringP <|> try coreP <|> try referenceP) <* eof

smallIntP :: Parser BB.Builder
smallIntP = do
    value <- chunk "w6[" *> L.decimal <* chunk "]"
    if value < 64
        then
            w8 $ 0x80 .|. value
        else
            fail $ "too big for a small unsigned integer: " <> show value

smallArrayP :: Parser BB.Builder
smallArrayP = do
    size <- chunk "#[" *> L.decimal <* chunk "]"
    w8 $ 0xC0 .|. size

literalBytesP :: Parser BB.Builder
literalBytesP = do
    chunk "0x"
    someBytes
  where
    someBytes :: Parser BB.Builder
    someBytes = do
        optional (single '-')
        byte <- byteP
        rest <- try someBytes <|> pure mempty
        pure $ BB.word8 byte <> rest

decimalP :: Parser BB.Builder
decimalP = do
    encodeExpr . encodeInt <$> L.decimal

stringSyntaxP :: Parser Text
stringSyntaxP = do
    single '"'
    (text, _) <- match $ many (anySingleBut '"')
    single '"'
    pure text

quotedStringP :: Parser Text
quotedStringP = do
    content <- stringSyntaxP
    pure $ '"' `T.cons` content `T.snoc` '"'

stringP :: Parser BB.Builder
stringP =
    encodeExpr . Array . LTE.encodeUtf8 . LT.fromStrict <$> stringSyntaxP

referenceP :: Parser BB.Builder
referenceP = qualified <|> unqualified
  where
    qualified = do
        ns <- T.pack <$> some (anySingleBut ':')
        single ':'
        name <- takeRest
        ref <- ensureRef ns name
        pure $ encodeExpr ref
    unqualified = do
        name <- takeRest
        ref <- ensureRef "<empty>" name
        pure $ encodeExpr ref

ensureRef :: Text -> Text -> Parser BULK
ensureRef nsMnemonic nameMnemonic = do
    ns <- ensureNamespace
    name <- ensureName ns
    pure $ Reference ns.marker name
  where
    ensureNamespace :: Parser Namespace
    ensureNamespace = do
        mNs <- gets lookupNS
        maybe createNamespace pure mNs
    ensureName :: Namespace -> Parser Int
    ensureName ns = maybe (createName ns) pure $ M.lookup nameMnemonic ns.usedNames
    lookupNS :: NamespaceMap -> Maybe Namespace
    lookupNS nss = M.lookup nsMnemonic nss.usedNamespaces
    createNamespace = do
        nss <- get
        let marker = nss.nextMarker
            newNamespace = Namespace{marker = marker, usedNames = M.empty, availableNames = [0 .. 255]}
        put nss{usedNamespaces = M.insert nsMnemonic newNamespace nss.usedNamespaces, nextMarker = marker + 1}
        pure newNamespace
    createName :: Namespace -> Parser Int
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

runParser :: (ParseErrorBundle Text String -> String) -> Parser a -> Text -> Either String a
runParser mapError parser =
    first mapError . flip evalState bulkProfile . runParserT parser "-"

runParserM :: (MonadState NamespaceMap m) => (ParseErrorBundle Text String -> String) -> Parser a -> Text -> m (Either String a)
runParserM mapError parser text = do
    result <- state $ runState $ runParserT parser "-" text
    pure $ first mapError result

debugParse :: (Show a) => Parser a -> Text -> IO ()
debugParse parser text = do
    putStrLn $ either id show (runParser errorBundlePretty parser text)
