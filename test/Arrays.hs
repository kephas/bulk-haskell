 module Arrays where

import           Data.BULK
import           Data.ByteString.Lazy           ( pack )
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Utils

smallArray :: Gen [Word8]
smallArray = resize 255 $ suchThat (listOf arbitraryByte) nonEmpty
 where
  nonEmpty [] = False
  nonEmpty _  = True

test_array_decoding = describe "reads arrays" $ do
  prop "reads short arrays" $ forAll smallArray $ \array ->
    readBin ([3, 4, asWord $ length array] ++ array ++ infinitePadding)
      `shouldBe` (Array $ pack array)
  where asWord = fromInteger . toInteger
