module Numbers where

import Data.Digits qualified as D
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Utils (lengthAsWord, readBin, smallArray, smallInt, toNums)

test_number_decoding :: SpecWith ()
test_number_decoding = describe "reads numbers" $ do
    prop "reads small ints" $ forAll smallInt $ \num ->
        toNums <$> readBin [1, 0x80 + num, 2] `shouldBe` Right [num]
    withCustom $ prop "reads ints in small arrays" $ forAll smallArray $ \array ->
        toNums <$> readBin ([1, 0xC0 + lengthAsWord array] ++ array ++ [2]) `shouldBe` Right [unDigits array]

unDigits :: [Word8] -> Integer
unDigits = D.unDigits 256 . map fromIntegral

withCustom :: SpecWith a -> SpecWith a
withCustom = modifyArgs (\args -> args{chatty = True, maxShrinks = 64})
