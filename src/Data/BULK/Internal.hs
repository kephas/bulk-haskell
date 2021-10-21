module Data.BULK.Internal
  ( BULK(..)
  ) where

import           Data.Bits                      ( Bits )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.LargeWord                 ( LargeKey(LargeKey)
                                                , Word128
                                                )
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word64
                                                , Word8
                                                )

data BULK = Nil
          | Form [BULK]
          | FormEnd
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
