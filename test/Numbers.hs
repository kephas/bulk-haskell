module Numbers where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Utils


test_number_decoding = describe "reads numbers" $ do
  prop "reads 8bits words" $ forAll (choose (0, 255)) $ \num ->
    (toNums $ readBin [1, 4, num, 2]) `shouldBe` [num]

  -- the logic is manually unrolled for big words on purpose,
  -- to avoid duplicating some logic that could be buggy the same way in code and tests
  prop "reads 16bits words"
    $ forAll (vectorOf 2 $ choose (0, 255))
    $ \[num1, num2] ->
        (toNums $ readBin [1, 5, num1, num2, 2])
          `shouldBe` [(fromIntegral num1) * 256 + (fromIntegral num2)]
  prop "reads 32bits words"
    $ forAll (vectorOf 4 $ choose (0, 255))
    $ \[num1, num2, num3, num4] ->
        (toNums $ readBin [1, 6, num1, num2, num3, num4, 2])
          `shouldBe` [ ((fromIntegral num1) * 256 * 256 * 256)
                       + ((fromIntegral num2) * 256 * 256)
                       + ((fromIntegral num3) * 256)
                       + (fromIntegral num4)
                     ]
  prop "reads 64bits words"
    $ forAll (vectorOf 8 $ choose (0, 255))
    $ \[num1, num2, num3, num4, num5, num6, num7, num8] ->
        ( toNums
          $ readBin [1, 7, num1, num2, num3, num4, num5, num6, num7, num8, 2]
          )
          `shouldBe` [ ( (fromIntegral num1)
                       * 256
                       * 256
                       * 256
                       * 256
                       * 256
                       * 256
                       * 256
                       )
                       + ( (fromIntegral num2)
                         * 256
                         * 256
                         * 256
                         * 256
                         * 256
                         * 256
                         )
                       + ((fromIntegral num3) * 256 * 256 * 256 * 256 * 256)
                       + ((fromIntegral num4) * 256 * 256 * 256 * 256)
                       + ((fromIntegral num5) * 256 * 256 * 256)
                       + ((fromIntegral num6) * 256 * 256)
                       + ((fromIntegral num7) * 256)
                       + (fromIntegral num8)
                     ]
