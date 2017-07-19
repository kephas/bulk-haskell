module Data.BULK ( BULK(..) ) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.LargeWord (Word128)
import qualified Data.ByteString as BS

data BULK = Nil
          | Form [BULK]
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
