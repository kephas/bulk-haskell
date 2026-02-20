{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.BULK.Decode (
    readFile,
    readFileV1,
    getExpression,
    getStream,
    parseStream,
    parseStreamV1,
    parseLazy,
    VersionConstraint (..),
    toNat,
) where

import Data.Binary.Get
import Data.Bits (shiftR, (.&.))
import Data.Bool (bool)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (eitherToMaybe)
import Data.Word (Word8)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Witch (from)
import Prelude hiding (readFile)

import Data.BULK.Types (BULK (..), Ref (..), pattern Core)
import Data.BULK.Utils (IOE, placeError, readFileLBS)

-- | Syntax token
data Syntax = FormEnd

-- | Version specification
data VersionConstraint = ReadVersion | Version1

-- | Read an entire file as a BULK stream
readFile :: FilePath -> IOE r BULK
readFile path = placeError path $ readFileLBS path >>= parseStream

readFileV1 :: FilePath -> IOE r BULK
readFileV1 path = placeError path $ readFileLBS path >>= parseStreamV1

-- | Parse an entire bytestring as a BULK stream
parseStream :: (Member (Error String) r) => LazyByteString -> Sem r BULK
parseStream = parseLazy $ getStream ReadVersion

parseStreamV1 :: (Member (Error String) r) => LazyByteString -> Sem r BULK
parseStreamV1 = parseLazy $ getStream Version1

-- | Get monad to read one BULK expression
getNext :: Get (Either Syntax BULK)
getNext = do
    marker <- getWord8
    case marker of
        0 -> pure $ Right Nil
        1 -> Right <$> getForm
        2 -> pure $ Left FormEnd
        3 -> getArray
        Prefix2 0b10 num -> pure $ Right $ Array $ BL.singleton num
        Prefix2 0b11 size -> Right . Array <$> getLazyByteString size
        _
            | marker `elem` [0x4 .. 0xF] -> do
                pos <- bytesRead
                fail $ show marker ++ " is a reserved marker byte (pos=" ++ show pos ++ ")"
            | marker `elem` [0x10 .. 0x7F] ->
                Right <$> getReference marker
            | otherwise -> fail "impossible"

getArray :: Get (Either Syntax BULK)
getArray = do
    msize <- parseIntegral <$> getNext
    case msize of
        Nothing -> fail "not a number (while reading an array size)"
        Just size ->
            Right . Array <$> getLazyByteString size

getReference :: Word8 -> Get BULK
getReference marker = do
    ns <- bool getSpecial pure (marker < 0x7F) $ fromIntegral marker
    Reference . Ref (from ns) . from <$> getWord8
  where
    getSpecial acc = do
        next <- getInt
        bool pure getSpecial (next == 0xFF) $ acc + next
    getInt :: Get Int
    getInt = fromIntegral <$> getWord8

data ParseContext = AtTopLevel | InForm

getSequence :: ParseContext -> Get [BULK]
getSequence context = do
    loop
  where
    loop = do
        eNext <- getNext
        finished <- isEmpty
        case (eNext, context, finished) of
            (Left FormEnd, AtTopLevel, _) -> fail "form end at top level"
            (Right _, InForm, True) -> fail "not enough data (while reading a form)"
            (Left FormEnd, InForm, _) -> pure []
            (Right next, AtTopLevel, True) -> pure [next]
            (Right next, _, False) -> (next :) <$> loop

getForm :: Get BULK
getForm = Form <$> getSequence InForm

-- | Get action to read a single BULK expression
getExpression :: Get BULK
getExpression = getNext >>= either (const $ fail "form end at top level") pure

-- | Get action to read an entire BULK stream
getStream :: VersionConstraint -> Get BULK
getStream Version1 = Form <$> getSequence AtTopLevel
getStream ReadVersion = do
    result@(first : _) <- getSequence AtTopLevel
    case first of
        Form [Core 0, Nat 1, Nat _] -> pure $ Form result
        Form [Core 0, Nat _, Nat _] -> fail "this application only supports BULK version 1.x"
        Form (Core 0 : _) -> fail "malformed version"
        _ -> fail "missing version"

parseIntegral :: (Integral a) => Either Syntax BULK -> Maybe a
parseIntegral eBulk =
    eitherToMaybe eBulk >>= toNat

-- | Extract a number from a raw BULK expression
toNat :: (Integral a) => BULK -> Maybe a
toNat bulk =
    case bulk of
        Array words_ -> Just $ BL.foldl addWord 0 words_
        _ -> Nothing
  where
    addWord num word = num * 256 + fromIntegral word

pattern Nat :: Int -> BULK
pattern Nat num <- (toNat -> Just num)

split26 :: (Integral a) => Word8 -> (Word8, a)
split26 byte = (shiftR (byte .&. 0xC0) 6, fromIntegral $ byte .&. 0x3F)

pattern Prefix2 :: (Integral a) => Word8 -> a -> Word8
pattern Prefix2 prefix value <- (split26 -> (prefix, value))

parseLazy :: (Member (Error String) r) => Get a -> LazyByteString -> Sem r a
parseLazy get input =
    case pushEndOfInput $ runGetIncremental get `pushChunks` input of
        Fail _unconsumed _offset err -> throw err
        Done _unconsumed _offset result -> pure result
        Partial _k -> throw "Impossible: parser wasn't failed or done after end of input!"
