module Utils where

import           Control.Exception              ( ErrorCall
                                                , handle
                                                )
import           Data.BULK                      ( BULK(Form)
                                                , getExpression
                                                , parseLazy
                                                , toIntegral
                                                )
import           Data.Binary.Get                ( runGet )
import           Data.ByteString.Lazy           ( pack )
import           Data.Either                    ( isLeft )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( catMaybes )
import           Data.Word                      ( Word8 )
import           Prelude                 hiding ( words )
import           System.Random                  ( Random )
import           Test.Hspec
import           Test.QuickCheck

readBin :: [Word8] -> Either String BULK
readBin = parseLazy getExpression . pack

readBin' :: [Word8] -> Either String BULK
readBin' = Right . runGet getExpression . pack


readFails :: [Word8] -> Expectation
readFails words = (isLeft $ readBin words) `shouldBe` True

shouldParseTo :: [Word8] -> BULK -> Expectation
words `shouldParseTo` expr = readBin words `shouldBe` Right expr

shouldParseTo' :: [Word8] -> BULK -> Expectation
words `shouldParseTo'` expr = readBin' words `shouldBe` Right expr

toNums :: Integral a => BULK -> [a]
toNums (Form exprs) = catMaybes $ map toIntegral exprs
toNums _            = error "not a form"

arbitraryByte :: (Num a, Random a) => Gen a
arbitraryByte = choose (0, 255)

infinitePadding :: [Word8]
infinitePadding = repeat 0

assertErrorCall :: IO a -> IO Bool
assertErrorCall action = handle handler $ action $> False
 where
  handler :: ErrorCall -> IO Bool
  handler _ = pure True
