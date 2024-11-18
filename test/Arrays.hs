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
        prop "reads smaller generic arrays" $ test_bigger_arrays_decoding 1
        prop "reads bigger generic arrays" $ withMaxSuccess 32 $ test_bigger_arrays_decoding 2
        prop "reads really big generic arrays" $ withMaxSuccess 4 $ test_bigger_arrays_decoding 3

test_bigger_arrays_decoding :: Int -> Property
test_bigger_arrays_decoding size =
    forAll (arraySizedWith size) $ \array ->
        ([3, 0xC0 + fromIntegral size] ++ asWords size (length array) ++ array) `shouldParseTo` Array (pack array)

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
