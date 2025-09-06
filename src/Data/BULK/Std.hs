module Data.BULK.Std where

import Data.BULK.Decode (BULK (Form, Reference))
import Data.BULK.Encode (encodeInt)

version :: Int -> Int -> BULK
version major minor =
    Form [Reference 16 0, encodeInt major, encodeInt minor]

define :: BULK -> BULK -> BULK
define ref value =
    Form [Reference 16 9, ref, value]
