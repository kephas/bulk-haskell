module Data.BULK.Decode (
    readFile,
    readFileWithVersion,
    getExpression,
    getStream,
    parseLazy,
    BULK (..),
    VersionConstraint (..),
    toIntegral,
) where

import Data.Binary.Get
import Data.Binary.Parser (parseLazy)
import Data.Bits (Bits, (.&.))
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (eitherToMaybe)
import Data.Word (
    Word8,
 )
import System.IO (
    Handle,
    stdin,
 )
import Prelude hiding (readFile)

-- | Raw BULK expression
data BULK
    = Nil
    | Form [BULK]
    | Array ByteString
    | Reference Int Int
    deriving (Eq, Ord, Show)

-- | Syntax token
data Syntax = FormEnd

-- | Version specification
data VersionConstraint = InStream | Version Int Int

-- | Read an entire file as a BULK stream
readFile :: FilePath -> IO (Either String BULK)
readFile = readFileWithVersion InStream

readFileWithVersion :: VersionConstraint -> FilePath -> IO (Either String BULK)
readFileWithVersion version path = parseLazy (getStream version) <$> BL.readFile path

-- | Get monad to read one BULK expression
getNext :: Get (Either Syntax BULK)
getNext = do
    marker <- getWord8
    case marker of
        0 -> pure $ Right Nil
        1 -> Right <$> getForm
        2 -> pure $ Left FormEnd
        3 -> getArray
        _
            | marker `elem` [0x4 .. 0xF] ->
                fail $ show marker ++ " is a reserved marker byte"
            | marker `elem` [0x10 .. 0x7F] ->
                Right <$> getReference marker
            | Just num <- with2bitPrefix 0x80 marker ->
                pure $ Right $ Array $ BL.singleton num
            | Just size <- with2bitPrefix 0xC0 marker ->
                Right . Array <$> getLazyByteString size
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
    Reference ns <$> getInt
  where
    getSpecial acc = do
        next <- getInt
        bool pure getSpecial (next == 0xFF) $ acc + next
    getInt = fromIntegral <$> getWord8

data ParseContext = AtTopLevel | InForm

getSequence :: ParseContext -> Get BULK
getSequence context = do
    loop []
  where
    loop results = do
        eNext <- getNext
        finished <- isEmpty
        case (eNext, context, finished) of
            (Left FormEnd, AtTopLevel, _) -> fail "form end at top level"
            (Left FormEnd, InForm, _) -> pure $ Form $ reverse results
            (Right next, AtTopLevel, True) -> pure $ Form $ reverse $ next : results
            (_, InForm, True) -> fail "not enough data (while reading a form)"
            (Right next, _, False) -> loop (next : results)

getForm = getSequence InForm

-- | Get action to read a single BULK expression
getExpression :: Get BULK
getExpression = getNext >>= either (const $ fail "form end at top level") pure

-- | Get action to read an entire BULK stream
getStream :: VersionConstraint -> Get BULK
getStream (Version 1 0) = getSequence AtTopLevel
getStream (Version _ _) = fail "this application only supports BULK version 1.0"
getStream InStream = do
    result@(Form (first : rest)) <- getSequence AtTopLevel
    case first of
        Form [Reference 16 0, Array major, Array minor]
            | [1] <- BL.unpack major
            , [0] <- BL.unpack minor ->
                pure result
        Form [Reference 16 0, _, _] -> fail "this application only supports BULK version 1.0"
        _ -> fail "missing version"

parseIntegral :: (Integral a) => Either Syntax BULK -> Maybe a
parseIntegral eBulk =
    eitherToMaybe eBulk >>= toIntegral

-- | Extract a number from a raw BULK expression
toIntegral :: (Integral a) => BULK -> Maybe a
toIntegral bulk =
    case bulk of
        Array words -> Just $ BL.foldl addWord 0 words
        _ -> Nothing
  where
    addWord num word = num * 256 + fromIntegral word

with2bitPrefix :: (Integral a) => Word8 -> Word8 -> Maybe a
with2bitPrefix prefix byte =
    if byte .&. 0xC0 == prefix
        then Just $ fromIntegral $ byte .&. 0x3F
        else Nothing
