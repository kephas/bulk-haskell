module Data.BULK.Math where

import Data.BULK.Internal

-- TODO: bigint
toIntegral :: Integral a => BULK -> Maybe a
toIntegral bulk = case bulk of
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
