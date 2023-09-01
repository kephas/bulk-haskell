module Data.BULK (
    BULK (..),
    getExpression,
    getStream,
    toIntegral,
    readFile,
    parseLazy,
) where

import Data.BULK.Decode (BULK (..), getExpression, getStream, parseLazy, readFile, toIntegral)
import Prelude hiding (readFile)
