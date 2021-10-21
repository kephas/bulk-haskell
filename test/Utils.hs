module Utils where

import           Data.BULK
import           Data.Binary.Get
import           Data.ByteString.Lazy           ( pack )
import           Data.Maybe

readBin = runGet getExpression . pack

toNums (Form exprs) = catMaybes $ map toIntegral exprs
