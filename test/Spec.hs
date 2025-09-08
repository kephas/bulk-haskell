{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.BULK
import Data.ByteString.Lazy (cons, pack, singleton)
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (nubBy)
import Data.String.Interpolate (i)
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, chooseInt, forAll, listOf, withMaxSuccess)
import Prelude hiding (readFile)

import Test.BULK.Decode
import Test.BULK.Encode ()

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
                    ((0xC0 + lengthAsWord array) `cons` pack array) `shouldParseTo` Array (pack array)
                prop "reads smaller generic arrays" $ test_bigger_arrays_decoding 1
                prop "reads bigger generic arrays" $ test_bigger_arrays_decoding 2
                prop "reads really big generic arrays" $ withMaxSuccess 8 $ test_bigger_arrays_decoding 3
            describe "reads numbers" $ do
                prop "reads small ints" $ forAll smallInt $ \num ->
                    readNum [0x80 + num] `shouldBe` Right num
                prop "reads ints in small arrays" $ forAll smallArray $ \array ->
                    readNum ((0xC0 + lengthAsWord array) : array) `shouldBe` Right (unDigits array)
            describe "read references" $ do
                prop "reads one-word marker references" $
                    forAll anySimpleRefBytes $ \(marker, ref) ->
                        [marker, ref] `shouldParseTo` Reference (fromIntegral marker) (fromIntegral ref)
                prop "reads two-words marker references" $
                    forAll anySimpleRefBytes $ \(marker, ref) ->
                        [0x7F, marker, ref] `shouldParseTo` Reference (0x7F + fromIntegral marker) (fromIntegral ref)
                prop "reads three-words marker references" $
                    forAll anySimpleRefBytes $ \(marker, ref) ->
                        [0x7F, 0xFF, marker, ref] `shouldParseTo` Reference (0x7F + 0xFF + fromIntegral marker) (fromIntegral ref)
            it "rejects reserved markers" $
                mapM_ (\marker -> readFails [marker, 0, 0, 0]) reservedMarkers
        describe "files" $ do
            it "reads simple files" $ do
                readFile "test/nesting.bulk"
                    `shouldReturn` Right (Form [version 1 0, Form [], Form [Nil, Form [Nil], Form []]])
                readFile "test/primitives.bulk"
                    `shouldReturn` Right
                        ( Form
                            [ version 1 0
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
                readFileWithVersion (SetVersion 1 0) "test/missing version.bulk" `shouldReturn` Right (Form [Nil])
        describe "version and profile" $ do
            it "checks for version 1.0" $ do
                readBinStream ReadVersion [1, 0x10, 0x00, 0x81, 0x81, 2] `shouldBe` Left "this application only supports BULK version 1.0"
                readBinStream ReadVersion [0] `shouldBe` Left "missing version"
                readBinStream (SetVersion 1 0) [0] `shouldBe` Right (Form [Nil])
                readBinStream (SetVersion 1 1) [0] `shouldBe` Left "this application only supports BULK version 1.0"
    describe "encoding" $ do
        it "encodes primitives" $ do
            encode [Nil, Form [], Array [], Reference 16 0] `shouldBe` [0, 1, 2, 0xC0, 16, 0]
        it "encodes numbers" $ do
            map encodeInt [0, 1, 255, 256] `shouldBe` [Array [0], Array [1], Array [255], Array [1, 0]]
            encode [Array [0], Array [1], Array [255], Array [1, 0]] `shouldBe` [0x80, 0x81, 0xC1, 0xFF, 0xC2, 0x01, 0x00]
        prop "round-trips arbitrary primitives" $ \expr ->
            encode [expr] `shouldParseTo` expr
    describe "text notation" $ do
        it "parses notation" $ do
            "nil" `shouldDenote` [Nil]
            "( )" `shouldDenote` [Form []]
            "( nil )" `shouldDenote` [Form [Nil]]
            "( nil ( nil nil ) nil )" `shouldDenote` [Form [Nil, Form [Nil, Nil], Nil]]
        it "parses small ints" $ for_ smallWords $ \word ->
            [i|w6[#{word}]|] `shouldDenote` [Array $ singleton word]
        it "parses small arrays" $ do
            "#[0]" `shouldDenote` [Array ""]
            "#[1] 0xAB" `shouldDenote` [Array "\xAB"]
            "#[2] 0xCDEF" `shouldDenote` [Array "\xCD\xEF"]
            "#[16] 0xaf5ac1b8-8e33-4025-97fe-f0e2030a00f7" `shouldDenote` [Array "\xaf\x5a\xc1\xb8\x8e\x33\x40\x25\x97\xfe\xf0\xe2\x03\x0a\x00\xf7"]
        it "parses small decimals" $ do
            for_ smallWords $ \word ->
                [i|#{word}|] `shouldDenote` [Array $ singleton word]
        prop "parses bigger decimals" $ forAll (chooseInt (64, maxBound)) $ \num ->
            [i|#{num}|] `shouldDenote` [encodeInt num]
        it "parses bulk core references" $ do
            "version" `shouldDenote` [Reference 16 0]
            "( version 1 0 ) true false ( define 0x1800 ( subst ( concat ( arg 1 ) ( arg 2 ) ) ) )" `shouldDenote` [version 1 0, core 1, core 2, Form [core 9, Reference 24 0, Form [core 0x11, Form [core 0x10, Form [core 0x12, Array [1]], Form [core 0x12, Array [2]]]]]]
            "( bulk:ns 0x1800 #[4] 0x0011-2233 ) ( bulk:ns-mnemonic 0x1800 )" `shouldDenote` [Form [core 6, Reference 24 0, Array "\x00\x11\x22\x33"], Form [core 0xB, Reference 24 0]]
        it "parses UTF-8 strings" $ do
            parseTextNotation [i|"foo"|] `shouldBe` Right "\xC3\&foo"
            parseTextNotation [i|"foo" "quuux"|] `shouldBe` Right "\xC3\&foo\xC5quuux"
            parseTextNotation [i|"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"|] `shouldBe` Right "\x03\xC1\78Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
            parseTextNotation [i|"関数型プログラミング"|] `shouldBe` Right [0xDE, 233, 150, 162, 230, 149, 176, 229, 158, 139, 227, 131, 151, 227, 131, 173, 227, 130, 176, 227, 131, 169, 227, 131, 159, 227, 131, 179, 227, 130, 176]
    describe "standard namespace" $ do
        it "has basic references" $ do
            version 1 0 `shouldBe` Form [Reference 16 0, Array [1], Array [0]]
        prop "has self-evaluating expressions" $ \expr ->
            eval [expr] `shouldBe` [expr]
        prop "has definitions" $ do
            rvs <- nubFst <$> listOf ((,) <$> anySimpleRef <*> arbitrary)
            let refs = map fst rvs
                values = map snd rvs
                definitions = zipWith define refs values
            pure $ eval (definitions ++ refs) `shouldBe` values

reservedMarkers :: [Word8]
reservedMarkers = [0x04 .. 0x0F]

test_bigger_arrays_decoding :: Int -> Property
test_bigger_arrays_decoding size =
    forAll (arraySizedWith size) $ \array ->
        encode [array] `shouldParseTo` array

nubFst :: (Eq a) => [(a, b)] -> [(a, b)]
nubFst = nubBy ((==) `on` fst)

smallWords :: [Word8]
smallWords = [0 .. 63]

core :: Int -> BULK
core = Reference 16
