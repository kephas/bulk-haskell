module Numbers where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Utils

pow256 :: Integer -> Integer
pow256 n = 256 ^ n

test_number_decoding :: SpecWith ()
test_number_decoding = describe "reads numbers" $ do
  prop "reads 8bits words" $ forAll arbitraryByte $ \num ->
    (toNums <$> readBin [1, 4, num, 2]) `shouldBe` Right [num]

  -- the logic is manually unrolled for big words on purpose,
  -- to avoid duplicating some logic that could be buggy the same way in code and tests
  prop "reads 16bits words"
    $ forAll (vectorOf 2 arbitraryByte)
    $ \[num1, num2] ->
        (toNums <$> readBin [1, 5, num1, num2, 2]) `shouldBe` Right
          [(fromIntegral num1) * pow256 1 + (fromIntegral num2)]
  prop "reads 32bits words"
    $ forAll (vectorOf 4 arbitraryByte)
    $ \[num1, num2, num3, num4] ->
        (toNums <$> readBin [1, 6, num1, num2, num3, num4, 2]) `shouldBe` Right
          [ ((fromIntegral num1) * pow256 3)
            + ((fromIntegral num2) * pow256 2)
            + ((fromIntegral num3) * pow256 1)
            + (fromIntegral num4)
          ]
  prop "reads 64bits words"
    $ forAll (vectorOf 8 arbitraryByte)
    $ \[num1, num2, num3, num4, num5, num6, num7, num8] ->
        (toNums <$> readBin
            [1, 7, num1, num2, num3, num4, num5, num6, num7, num8, 2]
          )
          `shouldBe` Right
                       [ ((fromIntegral num1) * pow256 7)
                         + ((fromIntegral num2) * pow256 6)
                         + ((fromIntegral num3) * pow256 5)
                         + ((fromIntegral num4) * pow256 4)
                         + ((fromIntegral num5) * pow256 3)
                         + ((fromIntegral num6) * pow256 2)
                         + ((fromIntegral num7) * pow256 1)
                         + (fromIntegral num8)
                       ]
  prop "reads 128bits words"
    $ forAll (vectorOf 16 arbitraryByte)
    $ \[num0, num1, num2, num3, num4, num5, num6, num7, num8, num9, numA, numB, numC, numD, numE, numF] ->
        (toNums <$> readBin
            [ 1
            , 8
            , num0
            , num1
            , num2
            , num3
            , num4
            , num5
            , num6
            , num7
            , num8
            , num9
            , numA
            , numB
            , numC
            , numD
            , numE
            , numF
            , 2
            ]
          )
          `shouldBe` Right
                       [ ((fromIntegral num0) * pow256 15)
                         + ((fromIntegral num1) * pow256 14)
                         + ((fromIntegral num2) * pow256 13)
                         + ((fromIntegral num3) * pow256 12)
                         + ((fromIntegral num4) * pow256 11)
                         + ((fromIntegral num5) * pow256 10)
                         + ((fromIntegral num6) * pow256 9)
                         + ((fromIntegral num7) * pow256 8)
                         + ((fromIntegral num8) * pow256 7)
                         + ((fromIntegral num9) * pow256 6)
                         + ((fromIntegral numA) * pow256 5)
                         + ((fromIntegral numB) * pow256 4)
                         + ((fromIntegral numC) * pow256 3)
                         + ((fromIntegral numD) * pow256 2)
                         + ((fromIntegral numE) * pow256 1)
                         + (fromIntegral numF)
                       ]
