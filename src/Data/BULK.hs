module Data.BULK (
    BULK (..),
    VersionConstraint (..),
    getExpression,
    getStream,
    toIntegral,
    readFileWithVersion,
    readFile,
    parseLazy,
    encode,
    encodeInt,
    version,
    define,
    eval,
) where

import Data.BULK.Decode (BULK (..), VersionConstraint (..), getExpression, getStream, parseLazy, readFile, readFileWithVersion, toIntegral)
import Data.BULK.Encode (encode, encodeInt)
import Data.BULK.Eval (eval)
import Data.BULK.Std (define, version)
import Prelude hiding (readFile)
