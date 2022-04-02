module References where

import           Data.BULK
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Utils

anySimpleMarker :: Gen Word8
anySimpleMarker = choose (32, 254)

anySimpleReference :: Gen (Word8, Word8)
anySimpleReference = (,) <$> anySimpleMarker <*> arbitraryByte

test_references :: SpecWith ()
test_references = describe "read references" $ do
  prop "reads one-word marker references"
    $ forAll anySimpleReference
    $ \(marker, ref) -> readBin [marker, ref]
        `shouldBe` Reference (fromIntegral marker) (fromIntegral ref)
