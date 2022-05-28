{-# LANGUAGE OverloadedStrings #-}

import           Arrays
import           Data.BULK
import           Numbers
import           Prelude                 hiding ( readFile )
import           References
import           Test.Hspec
import           Utils


main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = describe "BULK" $ do
  describe "decoding" $ do
    it "reads simple forms" $ do
      readBin [1, 2] `shouldBe` Form []
      readBin [1, 0, 2] `shouldBe` Form [Nil]
      readBin [1, 0, 1, 0, 2, 0, 2] `shouldBe` Form [Nil, Form [Nil], Nil]
    test_array_decoding
    test_number_decoding
    test_references
    it "rejects reserved markers"
      $ mapM_ (\marker -> readFails $ [marker] ++ infinitePadding) [14 .. 31]
  describe "files" $ do
    it "reads simple files" $ do
      readFile "test/nesting.bulk" `shouldReturn` Form
        [ Form [Reference 32 0, UnsignedWord8 1, UnsignedWord8 0]
        , Form []
        , Form [Nil, Form [Nil], Form []]
        ]
      readFile "test/primitives.bulk" `shouldReturn` Form
        [ Form [Reference 32 0, UnsignedWord8 1, UnsignedWord8 0]
        , Form
          [ Nil
          , Array "Hello world!"
          , UnsignedWord8 0x2A
          , UnsignedWord16 0x0100
          , UnsignedWord32 0x01000000
          , UnsignedWord64 0x0123456789ABCDEF
          , NegativeWord8 33
          , Reference 32  1
          , Reference 32  2
          , Reference 254 255
          , Reference 698 128
          ]
        ]

