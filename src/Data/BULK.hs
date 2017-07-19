module Data.BULK ( BULK(..) ) where

import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as BS

data BULK = Nil
          | Form [BULK]
          | Array BS.ByteString
          | UnsigneWord8 Word8
          | UnsigneWord16 Word16
          | UnsigneWord32 Word32
          | UnsigneWord64 Word64
          | SignedWord8 Word8
          | SignedWord16 Word16
          | SignedWord32 Word32
          | SignedWord64 Word64
