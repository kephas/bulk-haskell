module Data.BULK.Encode (encode)
where

import Data.BULK.Decode (BULK (..))
import Data.Word (Word8)

encode :: [BULK] -> [Word8]
encode [Nil] = [0]
