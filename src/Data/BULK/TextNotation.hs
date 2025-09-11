{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BULK.TextNotation where

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
import Data.Ord (Down (..))
import Data.Set (elems)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding.Error (OnDecodeError, lenientDecode)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ErrorFancy (ErrorFail), MonadParsec (..), ParseError (FancyError), ParseErrorBundle (..), Parsec, ShowErrorComponent (..), TraversableStream, VisualStream, anySingleBut, choice, chunk, errorBundlePretty, many, match, noneOf, oneOf, optional, parseMaybe, runParser, satisfy, single, some, (<|>))
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer qualified as L

import Data.BULK.Decode (BULK (..), VersionConstraint (ReadVersion), getStream, parseLazy)
import Data.BULK.Encode (encodeExpr, encodeInt)

parseTextNotation :: Text -> Either String ByteString
parseTextNotation source = do
    lexemes <- tryParser lexerP source
    BB.toLazyByteString . mconcat <$> traverse parseTextToken lexemes

parseTextFile :: FilePath -> IO (Either String BULK)
parseTextFile = parseTextFileWith lenientDecode ReadVersion

parseTextFileWith :: OnDecodeError -> VersionConstraint -> FilePath -> IO (Either String BULK)
parseTextFileWith onerror constraint file = do
    bytes <- B.readFile file
    pure $ parseTextNotation (LT.toStrict $ LTE.decodeUtf8With onerror bytes) >>= parseLazy (getStream constraint)

type Parser = Parsec String Text

tryParser :: Parser a -> Text -> Either String a
tryParser parser = first showFail . runParser parser "-"

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

parseTextToken :: Text -> Either String BB.Builder
parseTextToken "nil" = w8 0
parseTextToken "(" = w8 1
parseTextToken ")" = w8 2
parseTextToken token = tryParser tokenP token

w8 :: (Applicative f) => Word8 -> f BB.Builder
w8 = pure . BB.word8

tokenP :: Parser BB.Builder
tokenP = (smallIntP <|> smallArrayP <|> literalBytesP <|> decimalP <|> stringP <|> try coreP) <* eof

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

coreP :: Parser BB.Builder
coreP = optional (chunk "bulk:") >> choice parsers
  where
    -- they need to be sorted in reverse to try the longer ones before their prefix (e.g. try foo* before foo)
    parsers = map mkParser $ sortOn (Down . fst) $ zip (T.words "version true false stringenc iana-charset codepage ns package import define mnemonic/def ns-mnemonic verifiable-ns concat subst arg rest unsigned-int signed-int fraction binary-float decimal-float binary-fixed decimal-fixed decimal2 prefix-bytecode prefix-bytecode* postfix-bytecode postfix-bytecode* arity") ([0x0 .. 0xC] ++ [0x10 .. 0x13] ++ [0x20 .. 0x27] ++ [0x30 .. 0x34])
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

debugParse :: (Show a) => Parser a -> Text -> IO ()
debugParse parser text = do
    putStrLn $ either errorBundlePretty show (runParser parser "-" text)
