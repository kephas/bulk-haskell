{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.BULK
import Data.ByteString.Lazy (pack)
import Data.Word (Word8)
import Test.BULK.Decode
import Test.BULK.Encode ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, forAll, withMaxSuccess)
import Prelude hiding (readFile)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = describe "BULK" $ do
    describe "decoding" $ do
        describe "primitives" $ do
            it "reads simple forms" $ do
                [1, 2] `shouldParseTo` Form []
                [1, 0, 2] `shouldParseTo` Form [Nil]
                [1, 0, 1, 0, 2, 0, 2] `shouldParseTo` Form [Nil, Form [Nil], Nil]
            describe "arrays" $ do
                prop "reads small arrays" $ forAll smallArray $ \array ->
                    ((0xC0 + lengthAsWord array) : array) `shouldParseTo` Array (pack array)
                prop "reads smaller generic arrays" $ test_bigger_arrays_decoding 1
                prop "reads bigger generic arrays" $ withMaxSuccess 32 $ test_bigger_arrays_decoding 2
                prop "reads really big generic arrays" $ withMaxSuccess 4 $ test_bigger_arrays_decoding 3
            describe "reads numbers" $ do
                prop "reads small ints" $ forAll smallInt $ \num ->
                    toNums <$> readBin [1, 0x80 + num, 2] `shouldBe` Right [num]
                prop "reads ints in small arrays" $ forAll smallArray $ \array ->
                    toNums <$> readBin ([1, 0xC0 + lengthAsWord array] ++ array ++ [2]) `shouldBe` Right [unDigits array]
            describe "read references" $ do
                prop "reads one-word marker references" $
                    forAll anySimpleReference $ \(marker, ref) ->
                        [marker, ref] `shouldParseTo` Reference (fromIntegral marker) (fromIntegral ref)
                prop "reads two-words marker references" $
                    forAll anySimpleReference $ \(marker, ref) ->
                        [0x7F, marker, ref] `shouldParseTo` Reference (0x7F + fromIntegral marker) (fromIntegral ref)
                prop "reads three-words marker references" $
                    forAll anySimpleReference $ \(marker, ref) ->
                        [0x7F, 0xFF, marker, ref] `shouldParseTo` Reference (0x7F + 0xFF + fromIntegral marker) (fromIntegral ref)
            it "rejects reserved markers" $
                mapM_ (\marker -> readFails [marker, 0, 0, 0]) reservedMarkers
        describe "files" $ do
            it "reads simple files" $ do
                readFile "test/nesting.bulk"
                    `shouldReturn` Right (Form [bulk_v1_0, Form [], Form [Nil, Form [Nil], Form []]])
                readFile "test/primitives.bulk"
                    `shouldReturn` Right
                        ( Form
                            [ bulk_v1_0
                            , Form
                                [ Nil
                                , Array "Hello world!"
                                , Array [0x2A]
                                , Array []
                                , Array [0x40]
                                , Array [0x01, 0x00]
                                , Array [0x01, 0x00, 0x00, 0x00]
                                , Array [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF]
                                , Reference 0x18 0x01
                                , Reference 0x18 0x02
                                , Reference 0x7E 0xFF
                                , Reference (0x7F + 0xFF + 0xBC) 0x1A
                                ]
                            ]
                        )
            it "reports bad syntax" $ do
                readFile "test/bad nesting.bulk"
                    `shouldReturn` Left "not enough data (while reading a form)"
            it "checks for version 1.0" $ do
                readFile "test/missing version.bulk" `shouldReturn` Left "missing version"
                readFileWithVersion (Version 1 0) "test/missing version.bulk" `shouldReturn` Right (Form [Nil])
        describe "version and profile" $ do
            it "checks for version 1.0" $ do
                readBinStream InStream [1, 0x10, 0x00, 0x81, 0x81, 2] `shouldBe` Left "this application only supports BULK version 1.0"
                readBinStream InStream [0] `shouldBe` Left "missing version"
                readBinStream (Version 1 0) [0] `shouldBe` Right (Form [Nil])
                readBinStream (Version 1 1) [0] `shouldBe` Left "this application only supports BULK version 1.0"
    describe "encoding" $ do
        it "encodes primitives" $ do
            encode [Nil, Form [], Array [], Reference 16 0] `shouldBe` [0, 1, 2, 0xC0, 16, 0]
        it "encodes numbers" $ do
            map encodeInt [0, 1, 255, 256] `shouldBe` [Array [0], Array [1], Array [255], Array [1, 0]]
            encode [Array [0], Array [1], Array [255], Array [1, 0]] `shouldBe` [0x80, 0x81, 0xC1, 0xFF, 0xC2, 0x01, 0x00]
        prop "round-trips arbitrary primitives" $ \expr ->
            encode [expr] `shouldParseBSTo` expr

reservedMarkers :: [Word8]
reservedMarkers = [0x04 .. 0x0F]

test_bigger_arrays_decoding :: Int -> Property
test_bigger_arrays_decoding size =
    forAll (arraySizedWith size) $ \array ->
        ([3, 0xC0 + fromIntegral size] ++ asWords size (length array) ++ array) `shouldParseTo` Array (pack array)

bulk_v1_0 :: BULK
bulk_v1_0 = Form [Reference 16 0, Array [1], Array [0]]
