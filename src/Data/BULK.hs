{-# LANGUAGE StandaloneDeriving #-}
module Data.BULK ( BULK(..),
                   readBULK, readWhole,
                   hGetBULKExpression, hGetBULKStream,
                   getBULKExpression, getBULKStream
                 ) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.LargeWord (Word128, LargeKey(LargeKey))
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import System.IO (stdin)

data BULK = Nil
          | Form [BULK]
          | FormEnd
          | Array BS.ByteString
          | UnsignedWord8 Word8
          | UnsignedWord16 Word16
          | UnsignedWord32 Word32
          | UnsignedWord64 Word64
          | UnsignedWord128 Word128
          | SignedWord8 Word8
          | SignedWord16 Word16
          | SignedWord32 Word32
          | SignedWord64 Word64
          | SigneWord128 Word128
          | Reference Int Int
          | ParsingError Int

deriving instance Show BULK


readBULK = runGet $ readBULK' False
readWhole = runGet $ readFormPayload True

hGetBULKExpression handle = do readBULK <$> BS.hGetContents handle
hGetBULKStream handle = do readWhole <$> BS.hGetContents handle

getBULKExpression = hGetBULKExpression stdin
getBULKStream = hGetBULKStream stdin


readBULK' :: Bool -> Get BULK
readBULK' topLevel = do
  marker <- getWord8
  case marker of
    0 -> return Nil
    1 -> readFormPayload False
    2 -> return FormEnd
    4 -> UnsignedWord8 <$> getWord8
    5 -> UnsignedWord16 <$> getWord16be
    6 -> UnsignedWord32 <$> getWord32be
    7 -> UnsignedWord64 <$> getWord64be
    8 -> UnsignedWord128 <$> (LargeKey <$> getWord64be <*> getWord64be)
    _ -> Reference (fromIntegral marker) <$> (fromIntegral <$> getWord8)

readFormPayload :: Bool -> Get BULK
readFormPayload topLevel = do
  loop [] where
      loop results = do
            expression <- readBULK' topLevel
            position <- bytesRead
            case expression of
              FormEnd -> if topLevel then
                             return $ ParsingError $ fromIntegral position
                         else
                             return $ Form $ reverse results
              _ -> loop (expression:results)
