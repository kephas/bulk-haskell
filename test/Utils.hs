module Utils where

import Control.Exception (ErrorCall, handle)
import Data.BULK (BULK (Form), Version, getExpression, getStream, parseLazy, toIntegral)
import Data.ByteString.Lazy (pack)
import Data.Either (isLeft)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import System.Random (Random)
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (words)

readBin :: [Word8] -> Either String BULK
readBin = parseLazy getExpression . pack

readBinStream :: Version -> [Word8] -> Either String BULK
readBinStream version = parseLazy (getStream version) . pack

readFails :: [Word8] -> Expectation
readFails words = isLeft (readBin words) `shouldBe` True

shouldParseTo :: [Word8] -> BULK -> Expectation
words `shouldParseTo` expr = readBin words `shouldBe` Right expr

toNums :: (Integral a) => BULK -> [a]
toNums (Form exprs) = mapMaybe toIntegral exprs
toNums _ = error "not a form"

arbitraryByte :: (Num a, Random a) => Gen a
arbitraryByte = choose (0, 255)

assertErrorCall :: IO a -> IO Bool
assertErrorCall action = handle handler $ action $> False
  where
    handler :: ErrorCall -> IO Bool
    handler _ = pure True
