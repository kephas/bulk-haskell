import           Arrays
import           Data.BULK
import           Numbers
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

