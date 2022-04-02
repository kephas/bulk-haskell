module Utils where

import           Control.Exception              ( ErrorCall
                                                , handle
                                                )
import           Data.BULK                      ( BULK(Form)
                                                , getExpression
                                                , toIntegral
                                                )
import           Data.Binary.Get                ( runGet
                                                , runGetOrFail
                                                )
import           Data.ByteString.Lazy           ( pack )
import           Data.Either                    ( isLeft )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( catMaybes )
import           Data.Word                      ( Word8 )
import           System.Random                  ( Random )
import           Test.Hspec
import           Test.QuickCheck

readBin :: [Word8] -> BULK
readBin = runGet getExpression . pack

readFails :: [Word8] -> Expectation
readFails words =
  (isLeft $ runGetOrFail getExpression $ pack words) `shouldBe` True

toNums :: Integral a => BULK -> [a]
toNums (Form exprs) = catMaybes $ map toIntegral exprs

arbitraryByte :: (Num a, Random a) => Gen a
arbitraryByte = choose (0, 255)

infinitePadding :: [Word8]
infinitePadding = repeat 0

assertErrorCall :: IO a -> IO Bool
assertErrorCall action = handle handler $ action $> False
 where
  handler :: ErrorCall -> IO Bool
  handler _ = pure True
