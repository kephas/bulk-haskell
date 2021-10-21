import           Data.BULK
import           Numbers
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Utils

main :: IO ()
main = hspec spec

spec = describe "BULK" $ do
  describe "decoding" $ do
    it "reads simple forms" $ do
      readBin [1, 2] `shouldBe` Form []
      readBin [1, 0, 2] `shouldBe` Form [Nil]
      readBin [1, 0, 1, 0, 2, 0, 2] `shouldBe` Form [Nil, Form [Nil], Nil]
    test_number_decoding
