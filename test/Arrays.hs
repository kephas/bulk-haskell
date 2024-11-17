module Arrays (test_arrays_decoding) where

import Data.BULK
import Data.Bits (shiftR)
import Data.Bits.Extras (w8)
import Data.ByteString.Lazy (pack)
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, forAll, vectorOf, withMaxSuccess)
import Utils (arbitraryByte, lengthAsWord, shouldParseTo, smallArray)

test_arrays_decoding :: SpecWith ()
test_arrays_decoding = do
    describe "arrays" $ do
        prop "reads small arrays" $ forAll smallArray $ \array ->
            ((0xC0 + lengthAsWord array) : array) `shouldParseTo` Array (pack array)
        prop "reads smaller generic arrays" $ test_bigger_arrays_decoding 100 1
        prop "reads bigger generic arrays" $ test_bigger_arrays_decoding 32 2
        prop "reads really big generic arrays" $ test_bigger_arrays_decoding 4 3

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

test_bigger_arrays_decoding :: Int -> Int -> Property
test_bigger_arrays_decoding successes size =
    withMaxSuccess successes $ forAll (arraySizedWith size) $ \array ->
        ([3, 0xC0 + fromIntegral size] ++ asWords size (length array) ++ array) `shouldParseTo` Array (pack array)

asWords :: Int -> Int -> [Word8]
asWords size num = loop size num []
  where
    loop 0 _ acc = acc
    loop size' num' acc = loop (size' - 1) (shiftR num' 8) $ w8 num' : acc
