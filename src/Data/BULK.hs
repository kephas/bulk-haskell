module Data.BULK (
    BULK (..),
    getExpression,
    getStream,
    toIntegral,
    readFile,
    parseLazy,
) where

import Data.BULK.Decode (getExpression, getStream, parseLazy, readFile)
import Data.BULK.Internal (BULK (..))
import Data.BULK.Math (toIntegral)
import Prelude hiding (readFile)
