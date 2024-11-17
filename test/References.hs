module References where

import Data.BULK
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Utils

anySimpleMarker :: Gen Word8
anySimpleMarker = choose (0x10, 0x7E)

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
                [0x7F, marker, ref]
                    `shouldParseTo` Reference
                        (0x7F + fromIntegral marker)
                        (fromIntegral ref)
    prop "reads three-words marker references" $
        forAll anySimpleReference $
            \(marker, ref) ->
                [0x7F, 0xFF, marker, ref]
                    `shouldParseTo` Reference
                        (0x7F + 0xFF + fromIntegral marker)
                        (fromIntegral ref)
