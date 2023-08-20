module References where

import Data.BULK
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Utils

anySimpleMarker :: Gen Word8
anySimpleMarker = choose (32, 254)

anySimpleReference :: Gen (Word8, Word8)
anySimpleReference = (,) <$> anySimpleMarker <*> arbitraryByte

test_references :: SpecWith ()
test_references = describe "read references" $ do
    prop "reads one-word marker references" $
        forAll anySimpleReference $
            \(marker, ref) ->
                [marker, ref]
                    `shouldParseTo` Reference (fromIntegral marker) (fromIntegral ref)
    prop "reads two-words marker references" $
        forAll anySimpleReference $
            \(marker, ref) ->
                [255, marker, ref]
                    `shouldParseTo` Reference
                        (fromIntegral marker + 255)
                        (fromIntegral ref)
    prop "reads three-words marker references" $
        forAll anySimpleReference $
            \(marker, ref) ->
                [255, 255, marker, ref]
                    `shouldParseTo` Reference
                        (fromIntegral marker + 510)
                        (fromIntegral ref)
