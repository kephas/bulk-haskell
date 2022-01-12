module Utils where

import           Data.BULK                      ( BULK(Form)
                                                , getExpression
                                                , toIntegral
                                                )
import           Data.Binary.Get
import           Data.ByteString.Lazy           ( pack )
import           Data.Maybe
import           Data.Word
import           System.Random                  ( Random )
import           Test.QuickCheck

readBin :: [Word8] -> BULK
readBin = runGet getExpression . pack

toNums :: Integral a => BULK -> [a]
toNums (Form exprs) = catMaybes $ map toIntegral exprs

arbitraryByte :: (Num a, Random a) => Gen a
arbitraryByte = choose (0, 255)

infinitePadding :: [Word8]
infinitePadding = repeat 0
