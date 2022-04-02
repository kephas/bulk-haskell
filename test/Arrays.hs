 module Arrays where

import           Data.BULK
import           Data.Bits                      ( shiftR )
import           Data.Bits.Extras               ( w8 )
import           Data.ByteString.Lazy           ( pack )
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Utils

smallArray :: Int -> Gen [Word8]
smallArray sizeOrder = resize (2 ^ (sizeOrder * 8) - 1)
  $ suchThat (listOf arbitraryByte) nonEmpty
 where
  nonEmpty [] = False
  nonEmpty _  = True

-- TODO: fails with seed 2144970805
test_array_decoding :: SpecWith ()
test_array_decoding = describe "reads arrays" $ do
  prop "reads one-word sized arrays" $ forAll (smallArray 1) $ \array ->
    readBin ([3, 4, asWord $ length array] ++ array ++ infinitePadding)
      `shouldBe` (Array $ pack array)
  prop "reads two-words sized arrays" $ forAll (smallArray 2) $ \array ->
    readBin ([3, 5] ++ (asWords 2 $ length array) ++ array ++ infinitePadding)
      `shouldBe` (Array $ pack array)
  where asWord = fromInteger . toInteger

-- TODO: use when testing can take a long time
-- this use sizes of max 3 words encoded in 4, as to not exhaust memory
test_bigger_arrays_decoding :: SpecWith ()
test_bigger_arrays_decoding = describe "reads bigger arrays" $ do
  prop "reads four-words sized arrays" $ forAll (smallArray 3) $ \array ->
    readBin ([3, 6] ++ (asWords 4 $ length array) ++ array ++ infinitePadding)
      `shouldBe` (Array $ pack array)

asWords :: Int -> Int -> [Word8]
asWords size num = loop size num []
 where
  loop 0     _    acc = acc
  loop size' num' acc = loop (size' - 1) (shiftR num' 8) $ (w8 num') : acc
