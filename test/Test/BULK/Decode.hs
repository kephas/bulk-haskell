module Test.BULK.Decode where

import Control.Exception (ErrorCall, handle)
import Data.BULK (BULK (Form), VersionConstraint, getExpression, getStream, parseLazy, toIntegral)
import Data.Bits (shiftR)
import Data.Bits.Extras (w8)
import Data.ByteString.Lazy (ByteString, pack)
import Data.Digits qualified as D
import Data.Either (isLeft)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import System.Random (Random)
import Test.Hspec
import Test.QuickCheck (Gen, choose, listOf, resize, vectorOf)
import Prelude hiding (words)

readBin :: [Word8] -> Either String BULK
readBin = parseLazy getExpression . pack

readBinStream :: VersionConstraint -> [Word8] -> Either String BULK
readBinStream version = parseLazy (getStream version) . pack

readFails :: [Word8] -> Expectation
readFails words = isLeft (readBin words) `shouldBe` True

shouldParseTo :: [Word8] -> BULK -> Expectation
words `shouldParseTo` expr = readBin words `shouldBe` Right expr

shouldParseBSTo :: ByteString -> BULK -> Expectation
words `shouldParseBSTo` expr = parseLazy getExpression words `shouldBe` Right expr

toNums :: (Integral a) => BULK -> [a]
toNums (Form exprs) = mapMaybe toIntegral exprs
toNums _ = error "not a form"

unDigits :: [Word8] -> Integer
unDigits = D.unDigits 256 . map fromIntegral

arbitraryByte :: (Num a, Random a) => Gen a
arbitraryByte = choose (0, 255)

assertErrorCall :: IO a -> IO Bool
assertErrorCall action = handle handler $ action $> False
  where
    handler :: ErrorCall -> IO Bool
    handler _ = pure True

-- Arrays

smallInt :: (Num a, Random a) => Gen a
smallInt = choose (0, 63)

smallArray :: Gen [Word8]
smallArray = do
    resize 63 $ listOf arbitraryByte

lengthAsWord :: [a] -> Word8
lengthAsWord = fromIntegral . length

{- | Payload of a random array sized with an integer fitting in N
   | bytes (not a small array)
-}
arraySizedWith :: Int -> Gen [Word8]
arraySizedWith sizeOrder = do
    size <- choose (minSize, maxSize)
    vectorOf size arbitraryByte
  where
    minSize = if sizeOrder == 1 then 64 else 2 ^ ((sizeOrder - 1) * 8)
    maxSize = 2 ^ (sizeOrder * 8) - 1

{- | Convert an Int to its binary representation as N bytes

=== Example

>> asWords 3 511
[0, 1, 255]
-}
asWords :: Int -> Int -> [Word8]
asWords size num = loop size num []
  where
    loop 0 _ acc = acc
    loop size' num' acc = loop (size' - 1) (shiftR num' 8) $ w8 num' : acc

-- References

anySimpleMarker :: Gen Word8
anySimpleMarker = choose (0x10, 0x7E)

anySimpleReference :: Gen (Word8, Word8)
anySimpleReference = (,) <$> anySimpleMarker <*> arbitraryByte
