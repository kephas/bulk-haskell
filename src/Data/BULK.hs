module Data.BULK (
    BULK (..),
    Version (..),
    getExpression,
    getStream,
    toIntegral,
    readFileWithVersion,
    readFile,
    parseLazy,
    encode,
    encodeInt,
) where

import Data.BULK.Decode (BULK (..), Version (..), getExpression, getStream, parseLazy, readFile, readFileWithVersion, toIntegral)
import Data.BULK.Encode (encode, encodeInt)
import Prelude hiding (readFile)
