module Data.BULK.Decode
  ( hReadExpression
  , hReadStream
  , readExpression
  , readStream
  , getExpression
  , getStream
  ) where

import           Data.BULK.Internal
import           Data.BULK.Math
import           Data.Binary.Get
import qualified Data.ByteString.Lazy          as BL
import           Data.LargeWord                 ( LargeKey(LargeKey)
                                                , Word128
                                                )
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word64
                                                , Word8
                                                )
import           System.IO                      ( Handle
                                                , stdin
                                                )



hReadGet :: Get BULK -> Handle -> IO BULK
hReadGet get handle = (runGet get) <$> BL.hGetContents handle

-- | Read one BULK expression from the handle
hReadExpression :: Handle -> IO BULK
hReadExpression = hReadGet getExpression

-- | Read the entire BULK stream from the handle
hReadStream :: Handle -> IO BULK
hReadStream = hReadGet getStream

-- | Read one BULK expression from stdin
readExpression :: IO BULK
readExpression = hReadExpression stdin

-- | Read the entire BULK stream from stdin
readStream :: IO BULK
readStream = hReadStream stdin


-- | Get monad to read one BULK expression
getExpression :: Get BULK
getExpression = do
  marker <- getWord8
  case marker of
    0 -> pure Nil
    1 -> getForm
    2 -> pure FormEnd
    3 -> do
      msize <- toIntegral <$> getExpression
      case msize of
        Nothing   -> fail "not a number (while reading an array size)"
        Just size -> if size < 1
          then fail $ show size ++ " is not a proper array size"
          else Array <$> getLazyByteString size
    4  -> UnsignedWord8 <$> getWord8
    5  -> UnsignedWord16 <$> getWord16be
    6  -> UnsignedWord32 <$> getWord32be
    7  -> UnsignedWord64 <$> getWord64be
    8  -> UnsignedWord128 <$> (LargeKey <$> getWord64be <*> getWord64be)
    9  -> NegativeWord8 <$> getWord8
    10 -> NegativeWord16 <$> getWord16be
    11 -> NegativeWord32 <$> getWord32be
    12 -> NegativeWord64 <$> getWord64be
    13 -> NegativeWord128 <$> (LargeKey <$> getWord64be <*> getWord64be)
    _  -> Reference (fromIntegral marker) <$> (fromIntegral <$> getWord8)


data ParseContext = AtTopLevel | InForm

getSequence :: ParseContext -> Get BULK
getSequence context = do
  loop [] where
  loop results = do
    next     <- getExpression
    finished <- isEmpty
    case (next, context, finished) of
      (FormEnd, AtTopLevel, _    ) -> fail "form end at top level"
      (FormEnd, InForm    , _    ) -> pure $ Form $ reverse results
      (_      , AtTopLevel, True ) -> pure $ Form $ reverse $ next : results
      (_, InForm, True) -> fail "not enough data (while reading a form)"
      (_      , _         , False) -> loop (next : results)

getForm = getSequence InForm


-- | Get monad to read an entire BULK stream
getStream :: Get BULK
getStream = getSequence AtTopLevel

