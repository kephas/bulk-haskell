module Data.BULK.Decode (
    readFile,
    readFileWithVersion,
    getExpression,
    getStream,
    parseLazy,
    BULK (..),
    Version (..),
    toIntegral,
) where

import Data.Binary.Get
import Data.Binary.Parser (parseLazy)
import Data.Bits (Bits)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Either.Extra (eitherToMaybe)
import Data.LargeWord (
    LargeKey (LargeKey),
    Word128,
 )
import Data.Word (
    Word16,
    Word32,
    Word64,
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
    | UnsignedWord8 Word8
    | UnsignedWord16 Word16
    | UnsignedWord32 Word32
    | UnsignedWord64 Word64
    | UnsignedWord128 Word128
    | NegativeWord8 Word8
    | NegativeWord16 Word16
    | NegativeWord32 Word32
    | NegativeWord64 Word64
    | NegativeWord128 Word128
    | Reference Int Int
    deriving (Eq, Ord, Show)

-- | Syntax token
data Syntax = FormEnd

-- | Version specification
data Version = InStream | Version Int Int

-- | Read an entire file as a BULK stream
readFile :: FilePath -> IO (Either String BULK)
readFile = readFileWithVersion InStream

readFileWithVersion :: Version -> FilePath -> IO (Either String BULK)
readFileWithVersion version path = parseLazy (getStream version) <$> BL.readFile path

getWord128be :: Get Word128
getWord128be = flip LargeKey <$> getWord64be <*> getWord64be

-- | Get monad to read one BULK expression
getNext :: Get (Either Syntax BULK)
getNext = do
    marker <- getWord8
    case marker of
        0 -> pure $ Right Nil
        1 -> Right <$> getForm
        2 -> pure $ Left FormEnd
        3 -> do
            msize <- parseIntegral <$> getNext
            case msize of
                Nothing -> fail "not a number (while reading an array size)"
                Just size ->
                    if size < 1
                        then fail $ show size ++ " is not a proper array size"
                        else Right . Array <$> getLazyByteString size
        4 -> Right . UnsignedWord8 <$> getWord8
        5 -> Right . UnsignedWord16 <$> getWord16be
        6 -> Right . UnsignedWord32 <$> getWord32be
        7 -> Right . UnsignedWord64 <$> getWord64be
        8 -> Right . UnsignedWord128 <$> getWord128be
        9 -> Right . NegativeWord8 <$> getWord8
        10 -> Right . NegativeWord16 <$> getWord16be
        11 -> Right . NegativeWord32 <$> getWord32be
        12 -> Right . NegativeWord64 <$> getWord64be
        13 -> Right . NegativeWord128 <$> getWord128be
        _
            | marker < 32 -> fail $ show marker ++ " is a reserved marker byte"
            | otherwise -> Right <$> getReference marker

getReference :: Word8 -> Get BULK
getReference = go 0
  where
    go acc current =
        let acc' = acc + fromIntegral current
         in if current == 255
                then go acc' =<< getWord8
                else Reference acc' . fromIntegral <$> getWord8

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
getStream :: Version -> Get BULK
getStream (Version 1 0) = getSequence AtTopLevel
getStream (Version _ _) = fail "bad version"
getStream InStream = do
    result@(Form (first : rest)) <- getSequence AtTopLevel
    case first of
        Form [Reference 32 0, UnsignedWord8 1, UnsignedWord8 0] -> pure result
        Form [Reference 32 0, _, _] -> fail "bad version"
        _ -> fail "missing version"

parseIntegral :: Integral a => Either Syntax BULK -> Maybe a
parseIntegral eBulk = do
    bulk <- eitherToMaybe eBulk
    case bulk of
        UnsignedWord8 word -> Just $ fromIntegral word
        UnsignedWord16 word -> Just $ fromIntegral word
        UnsignedWord32 word -> Just $ fromIntegral word
        UnsignedWord64 word -> Just $ fromIntegral word
        UnsignedWord128 word -> Just $ fromIntegral word
        NegativeWord8 word -> Just $ negate $ fromIntegral word
        NegativeWord16 word -> Just $ negate $ fromIntegral word
        NegativeWord32 word -> Just $ negate $ fromIntegral word
        NegativeWord64 word -> Just $ negate $ fromIntegral word
        NegativeWord128 word -> Just $ negate $ fromIntegral word
        _ -> Nothing

-- | Extract a number from a raw BULK expression
toIntegral :: Integral a => BULK -> Maybe a
toIntegral = parseIntegral . Right
