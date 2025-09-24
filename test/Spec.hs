{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Data.ByteString.Lazy (ByteString, pack, singleton)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (nubBy)
import Data.String.Interpolate (i)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, chooseInt, forAll, listOf, withMaxSuccess)
import Prelude hiding (readFile)

import Data.BULK
import Test.BULK.Decode
import Test.BULK.Encode
import Test.QuickCheck.Instances.BULK ()

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = describe "BULK" $ do
    describe "fast tests" $ do
        --
        -- Decoding
        describe "decoding" $ do
            describe "primitives" $ do
                it "reads simple forms" $ do
                    "\1\2" `shouldParseTo` Form []
                    "\1\0\2" `shouldParseTo` Form [Nil]
                    "\1\0\1\0\2\0\2" `shouldParseTo` Form [Nil, Form [Nil], Nil]
                describe "arrays" $ do
                    prop "reads small arrays" $ forAll smallArray $ \array ->
                        encodeSmallArray array `shouldParseTo` Array (pack array)
                    prop "reads smaller generic arrays" $ test_bigger_arrays_decoding 1
                    prop "reads bigger generic arrays" $ test_bigger_arrays_decoding 2
                describe "reads numbers" $ do
                    prop "reads small ints" $ forAll smallInt $ \num ->
                        encodeSmallInt num `shouldParseToNat` num
                    prop "reads ints in small arrays" $ forAll smallArray $ \array ->
                        encodeSmallArray array `shouldParseToNat` unDigits array
                describe "read references" $ do
                    prop "reads one-word marker references" $
                        forAll anySimpleRefBytes $ \(marker, ref) ->
                            pack [marker, ref] `shouldParseTo` Reference (fromIntegral marker) (fromIntegral ref)
                    prop "reads two-words marker references" $
                        forAll anySimpleRefBytes $ \(marker, ref) ->
                            pack [0x7F, marker, ref] `shouldParseTo` Reference (0x7F + fromIntegral marker) (fromIntegral ref)
                    prop "reads three-words marker references" $
                        forAll anySimpleRefBytes $ \(marker, ref) ->
                            pack [0x7F, 0xFF, marker, ref] `shouldParseTo` Reference (0x7F + 0xFF + fromIntegral marker) (fromIntegral ref)
                it "rejects reserved markers" $
                    traverse_ readFailsOn reservedMarkers
            describe "files" $ do
                it "reads simple files" $ do
                    readFile "test/nesting.bulk" `shouldReturn` nesting
                    readFile "test/primitives.bulk" `shouldReturn` primitives
                it "reports bad syntax" $ do
                    readFile "test/bad nesting.bulk" `shouldReturn` badNesting
                it "checks for version 1.0" $ do
                    readFile "test/missing version.bulk" `shouldReturn` Left "missing version"
                    readFileWithVersion (SetVersion 1 0) "test/missing version.bulk" `shouldReturn` Right [Nil]
            describe "version and profile" $ do
                it "checks for version 1.0" $ do
                    parseStreamWith ReadVersion "\x01\x10\x00\x81\x80\x02" `shouldBe` Right [Form [Reference 16 0, 1, 0]]
                    parseStreamWith ReadVersion "\x01\x10\x00\x81\x82\x02" `shouldBe` Right [Form [Reference 16 0, 1, 2]]
                    parseStreamWith ReadVersion "\x01\x10\x00\x82\x80\x02" `shouldBe` Left "this application only supports BULK version 1.x"
                    parseStreamWith ReadVersion "\x01\x10\x00\x81\x00\x00\x02" `shouldBe` Left "malformed version"
                    parseStreamWith ReadVersion "\0" `shouldBe` Left "missing version"
                    parseStreamWith (SetVersion 1 0) "\0" `shouldBe` Right [Nil]
                    parseStreamWith (SetVersion 1 2) "\0" `shouldBe` Right [Nil]
                    parseStreamWith (SetVersion 2 0) "\0" `shouldBe` Left "this application only supports BULK version 1.x"
        --
        -- Encoding
        describe "encoding" $ do
            it "encodes primitives" $ do
                encode [Nil, Form [], Array "", Reference 16 0] `shouldBe` "\x00\x01\x02\xC0\x10\x00"
            it "encodes natural numbers" $ do
                map @Int encodeNat [0, 1, 0xFF, 0x100, 0xFFFF, 0x1_0000] `shouldBe` [Array "\0", Array "\1", Array "\xFF", Array "\1\0", Array "\xFF\xFF", Array "\0\1\0\0"]
                map @Integer encodeNat [0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF] `shouldBe` [Array "\x00\x00\x00\x00\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"]
                encode [Array "\0", Array "\1", Array "\xFF", Array "\1\0"] `shouldBe` "\x80\x81\xC1\xFF\xC2\x01\x00"
            prop "round-trips arbitrary primitives" $ \expr ->
                encode [expr] `shouldParseTo` expr
        --
        -- Text Notation
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
                [i|#{num}|] `shouldDenote` [encodeNat num]
            it "parses bulk core references" $ do
                "version" `shouldDenote` [Reference 16 0]
                "( version 1 0 ) true false ( define 0x1800 ( subst ( concat ( arg 1 ) ( arg 2 ) ) ) )" `shouldDenote` [version 1 0, core 1, core 2, Form [core 6, Reference 24 0, Form [core 0x0B, Form [core 0x0A, Form [core 0x0C, Array "\1"], Form [core 0x0C, Array "\2"]]]]]
                "( bulk:ns 0x1800 #[4] 0x0011-2233 ) ( bulk:ns-mnemonic 0x1800 )" `shouldDenote` [Form [core 3, Reference 24 0, Array "\x00\x11\x22\x33"], Form [core 8, Reference 24 0]]
            it "parses UTF-8 strings" $ do
                parseTextNotation [i|"foo"|] `shouldBe` Right "\xC3\&foo"
                parseTextNotation [i|"foo" "quuux"|] `shouldBe` Right "\xC3\&foo\xC5quuux"
                parseTextNotation [i|"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"|] `shouldBe` Right "\x03\xC1\78Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
                parseTextNotation [i|"関数型プログラミング"|] `shouldBe` Right "\xDE\233\150\162\230\149\176\229\158\139\227\131\151\227\131\173\227\130\176\227\131\169\227\131\159\227\131\179\227\130\176"
            it "parses example files" $ do
                parseTextFile "test/nesting.bulktext" `shouldReturn` nesting
                parseTextFile "test/primitives.bulktext" `shouldReturn` primitives
                parseTextFile "test/bad nesting.bulktext" `shouldReturn` badNesting
            it "parses unknown references" $ do
                "foo:bar quux:one foo:baz" `shouldDenote` (uncurry Reference <$> [(24, 0), (25, 0), (24, 1)])
        --
        -- Core namespace and evaluation
        describe "core namespace" $ do
            it "has basic references" $ do
                version 1 0 `shouldBe` Form [Reference 16 0, Array "\1", Array "\0"]
            prop "has self-evaluating expressions" $ \expr ->
                eval [expr] `shouldBe` [expr]
            prop "has definitions" $ do
                rvs <- nubBy ((==) `on` fst) <$> listOf ((,) <$> anySimpleRef <*> arbitrary)
                let refs = map fst rvs
                    values = map snd rvs
                    definitions = zipWith define refs values
                pure $ eval (definitions ++ refs) `shouldBe` values
            describe "parses numbers" $ do
                it "small ints" $ for_ smallWords \w ->
                    encodeSmallInt w `shouldParseToInt` fromIntegral w
                it "typed Int forms" $ do
                    parseInts $ parseOnlyIntCases ++ bidirectionalIntCases
                    parseInts bigIntCases
            describe "encodes numbers" $ do
                it "ints" $ do
                    encodeInt @Int (-1) `shouldBe` Form [core 0x21, Array "\xFF"]
                    for_ bidirectionalIntCases \(kind, bytes, value) ->
                        encodeInt value `shouldBe` Form [core kind, Array bytes]
    describe "slow tests" $ do
        prop "reads really big generic arrays" $ withMaxSuccess 20 $ test_bigger_arrays_decoding 3

nesting, primitives, badNesting :: Either String [BULK]
nesting = Right [version 1 0, Form [], Form [Nil, Form [Nil], Form []]]
primitives =
    Right
        [ version 1 0
        , Form
            [ Nil
            , Array "Hello world!"
            , Array "\x2A"
            , Array ""
            , Array "\x40"
            , Array "\x01\x00"
            , Array "\x01\x00\x00\x00"
            , Array "\x01\x23\x45\x67\x89\xAB\xCD\xEF"
            , Reference 0x18 0x01
            , Reference 0x18 0x02
            , Reference 0x7E 0xFF
            , Reference (0x7F + 0xFF + 0xBC) 0x1A
            ]
        ]
badNesting = Left "not enough data (while reading a form)"

parseOnlyIntCases, bidirectionalIntCases :: [(Int, ByteString, Int)]
parseOnlyIntCases =
    [ (0x20, "\x01", 0x1)
    , (0x21, "\x00\x01", 0x1)
    , (0x21, "\xFF\xFF", -0x1)
    , (0x21, "\x00\x00\x00\x01", 0x1)
    , (0x21, "\xFF\xFF\xFF\xFF", -0x1)
    , (0x21, "\x00\x00\x00\x00\x00\x00\x00\x01", 0x1)
    , (0x21, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", -0x1)
    ]
bidirectionalIntCases =
    [ (0x21, "\x01", 0x1)
    , (0x21, "\x7F", 0x7F)
    , (0x21, "\xFF", -0x1)
    , (0x21, "\x80", -0x80)
    , (0x21, "\x81", -0x7F)
    , (0x21, "\x7F\xFF", 0x7FFF)
    , (0x21, "\x80\x00", -0x8000)
    , (0x21, "\x80\x01", -0x7FFF)
    , (0x21, "\x7F\xFF\xFF\xFF", 0x7FFF_FFFF)
    , (0x21, "\x80\x00\x00\x00", -0x8000_0000)
    , (0x21, "\x80\x00\x00\x01", -0x7FFF_FFFF)
    , (0x21, "\x7F\xFF\xFF\xFF\xFF\xFF\xFF\xFF", 0x7FFF_FFFF_FFFF_FFFF)
    , (0x21, "\x80\x00\x00\x00\x00\x00\x00\x00", -0x8000_0000_0000_0000)
    , (0x21, "\x80\x00\x00\x00\x00\x00\x00\x01", -0x7FFF_FFFF_FFFF_FFFF)
    ]

bigIntCases :: [(Int, ByteString, Integer)]
bigIntCases =
    [(0x21, "\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", -0x8000_0000_0000_0000_0000_0000_0000_0000)]
