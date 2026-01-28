{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, chooseInt, forAll, listOf)
import Witch (from, via)
import Prelude hiding (readFile)

import Data.BULK
import Data.BULK.Encode (pattern IntReference)
import Data.BULK.Types (pattern Core)
import Data.Text (Text)
import Test.BULK.Decode
import Test.QuickCheck.Instances.BULK (simpleBULK)

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
                            pack [marker, ref] `shouldParseTo` Reference (Name (via @Int marker) (fromIntegral ref))
                    prop "reads two-words marker references" $
                        forAll anySimpleRefBytes $ \(marker, ref) ->
                            pack [0x7F, marker, ref] `shouldParseTo` Reference (Name (from @Int $ 0x7F + fromIntegral marker) (fromIntegral ref))
                    prop "reads three-words marker references" $
                        forAll anySimpleRefBytes $ \(marker, ref) ->
                            pack [0x7F, 0xFF, marker, ref] `shouldParseTo` Reference (Name (from @Int $ 0x7F + 0xFF + fromIntegral marker) (fromIntegral ref))
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
                    readFileWithVersion (SetVersion 1 0) "test/missing version.bulk" `shouldReturn` Right (Form [Nil])
            describe "version and profile" $ do
                it "checks for version 1.0" $ do
                    parseStreamWith ReadVersion "\x01\x10\x00\x81\x80\x02" `shouldBe` Right (Form [Form [IntReference 16 0, 1, 0]])
                    parseStreamWith ReadVersion "\x01\x10\x00\x81\x82\x02" `shouldBe` Right (Form [Form [IntReference 16 0, 1, 2]])
                    parseStreamWith ReadVersion "\x01\x10\x00\x82\x80\x02" `shouldBe` Left "this application only supports BULK version 1.x"
                    parseStreamWith ReadVersion "\x01\x10\x00\x81\x00\x00\x02" `shouldBe` Left "malformed version"
                    parseStreamWith ReadVersion "\0" `shouldBe` Left "missing version"
                    parseStreamWith (SetVersion 1 0) "\0" `shouldBe` Right (Form [Nil])
                    parseStreamWith (SetVersion 1 2) "\0" `shouldBe` Right (Form [Nil])
                    parseStreamWith (SetVersion 2 0) "\0" `shouldBe` Left "this application only supports BULK version 1.x"
        --
        -- Encoding
        describe "encoding" $ do
            it "encodes primitives" $ do
                encode [Nil, Form [], Array "", IntReference 16 0] `shouldBe` "\x00\x01\x02\xC0\x10\x00"
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
            it "parses arrays" $ do
                "# 64 0x0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000" `shouldDenote` [Array $ pack $ replicate 64 0]
            prop "parses bigger decimals" $ forAll (chooseInt (64, maxBound)) $ \num ->
                [i|#{num}|] `shouldDenote` [encodeNat num]
            it "parses bulk core references" $ do
                "version" `shouldDenote` [IntReference 16 0]
                "( version 1 0 ) true false ( define 0x1400 ( subst ( concat ( arg 1 ) ( arg 2 ) ) ) )" `shouldDenote` [version 1 0, Core 1, Core 2, Form [Core 6, IntReference 0x14 0, Form [Core 0x0B, Form [Core 0x0A, Form [Core 0x0C, Array "\1"], Form [Core 0x0C, Array "\2"]]]]]
                "( bulk:ns 0x1400 #[4] 0x0011-2233 ) ( bulk:ns-mnemonic 0x1400 )" `shouldDenote` [Form [Core 3, IntReference 0x14 0, Array "\x00\x11\x22\x33"], Form [Core 8, IntReference 0x14 0]]
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
                "foo:bar quux:one foo:baz" `shouldDenote` (uncurry IntReference <$> [(0x14, 0), (0x15, 0), (0x14, 1)])
                "123:one" `shouldDenote` [IntReference 0x14 0]
        --
        -- Core namespace and evaluation
        describe "core namespace" $ do
            it "has basic references" $ do
                version 1 0 `shouldBe` Form [IntReference 16 0, Array "\1", Array "\0"]
            prop "has definitions" $ do
                rvs <- nubBy ((==) `on` fst) <$> listOf ((,) <$> anySimpleRef <*> arbitrary)
                let refs = map fst rvs
                    values = map snd rvs
                    definitions = zipWith define refs values
                pure $ eval [] (Form $ definitions ++ refs) `shouldBe` Right (Form values)
            it "respect scoping rule" do
                shouldFail $ decodeNotation @[[Int]] [] "( ( bulk:define 0x1400 42 ) 0x1400 ) ( 0x1400 )"
            describe "parses numbers" $ do
                it "small ints" $ for_ smallWords \w ->
                    encodeSmallInt w `shouldParseToInt` fromIntegral w
                it "typed Int forms" $ do
                    parseInts $ parseOnlyIntCases ++ bidirectionalIntCases
                    parseInts bigIntCases
            describe "encodes numbers" $ do
                it "ints" $ do
                    encodeInt @Int (-1) `shouldBe` Form [Core 0x21, Array "\xFF"]
                    for_ bidirectionalIntCases \(kind, bytes, value) ->
                        encodeInt value `shouldBe` Form [Core kind, Array bytes]
            it "has verifiable namespaces" $ do
                decodeNotationFile @[()] [hash0] "test/123-bad.bulktext" `shouldReturn` Left "verification failed for namespace: 123 (expected digest 00000000000000000000000000000000 but got dd3bff1608fa25cc16ba90c0f8b4976e4a50b1d215cf8448e890e7cc4a4b0ff0)"
                decodeNotationFile @[Int] [hash0] "test/123.bulktext" `shouldReturn` Right [1, 2, 3]
            it "can bootstrap hashing" $ do
                decodeNotationFile @[()] [hash0] "test/bootstrap-bad.bulktext" `shouldReturn` Left "unable to bootstrap namespace: bootstrap"
                decodeNotationFile @[()] [hash0] "config/hash0.bulktext" `shouldReturn` Right []
            it "has verifiable packages" $ do
                decodeNotationFile @[()] [hash0] "test/package-bad.bulktext" `shouldReturn` Left "verification failed for package (expected digest 00000000000000000000000000000000 but got a67d83372222b4bd8da851daa24e034b6a3ef3a50b69931b61966aa36a33ff1a)"

        --
        -- Parser monad
        describe "Parser monad" $ do
            it "parses Haskell values" $ do
                decodeNotation [hash0, foo] "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true 42 )" `shouldBe` Right [Foo False True 42]
                decodeNotation @[Foo] [hash0, foo] "( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true 42 )" `shouldBe` Left "missing version"
                decodeNotation @[Foo] [hash0, foo] "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true nil )" `shouldBe` Left "cannot parse as integer: Nil"
                decodeNotation @[Foo] [hash0, foo] "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true )" `shouldBe` Left "no next BULK expression"
                decodeFile [hash0, foo] "test/foo.bulk" `shouldReturn` Right [Foo False True 42]
                decodeFile [hash0, foo] "test/foos.bulk" `shouldReturn` Right [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotationFile [hash0, foo] "test/foos.bulktext" `shouldReturn` Right [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotation [hash0, foo, bar] "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) )  ( ns 22 ( hash0:shake128 #[4] 0x6744BA37 ) ) ( 0x16-00 1 ( 0x15-00 false true 42 ) )" `shouldBe` Right [Bar 1 (Foo False True 42)]
                decodeNotation [hash0, foo, bar] "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( package ( hash0:shake128 #[4] 0xD4912423 ) ( hash0:shake128 #[4] 0x117A63BB ) ( hash0:shake128 #[4] 0x6744BA37 ) ) ( import 21 2 ( hash0:shake128 #[4] 0xD4912423 ) ) ( 0x16-00 1 ( 0x15-00 false true 42 ) )" `shouldBe` Right [Bar 1 (Foo False True 42)]

        --
        -- Custom encoders
        describe "Custom encoders" $ do
            it "encodes simple primitive types" $ do
                matchTo [(True, Core 0x01), (False, Core 0x02)]
                matchTo @Int [(1, Array "\1"), (64, Array "\64"), (256, Array "\x01\x00"), (-1, Form [Core 0x21, Array "\xFF"])]
                matchTo [([True, False], Form [Core 0x01, Core 0x02])]
                matchTo @ByteString [("", Array ""), ("\0\1\2", Array "\0\1\2")]
            it "encodes UTF-8 strings" $ do
                matchTo @Text [("foo", Array "foo"), ("γράφω", Array "\xCE\xB3\xCF\x81\xCE\xAC\xCF\x86\xCF\x89")]

    describe "slow tests" $ do
        prop "reads really big generic arrays" $ test_bigger_arrays_decoding 3
        prop "has self-evaluating expressions" $ forAll (listOf simpleBULK) $ \exprs ->
            eval [] (Form exprs) `shouldBe` Right (Form exprs)

nesting, primitives, badNesting :: Either String BULK
nesting = Right $ Form [version 1 0, Form [], Form [Nil, Form [Nil], Form []]]
primitives =
    Right $
        Form
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
                , IntReference 0x14 0x01
                , IntReference 0x14 0x02
                , IntReference 0x7E 0xFF
                , IntReference (0x7F + 0xFF + 0xBC) 0x1A
                ]
            ]
badNesting = Left "not enough data (while reading a form)"

parseOnlyIntCases, bidirectionalIntCases :: [(Word8, ByteString, Int)]
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

bigIntCases :: [(Word8, ByteString, Integer)]
bigIntCases =
    [(0x21, "\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", -0x8000_0000_0000_0000_0000_0000_0000_0000)]

hash0 :: NamespaceDefinition
hash0 =
    NamespaceDefinition
        { matchID = MatchNamePrefix 0x00 "\xE2\xEC\xDA\x49\x4A\x78\x19\x5B\x07\x03\x4A\xB1\x0B\x2C\x43\x90\xC9\x4C\x01\x25\x1B\x13\xD1\xA4\x83\xD2\x1E\x8B\x78\x1D\x70\x3D"
        , mnemonic = "hash0"
        , names =
            [ DigestName{marker = 0x00, mnemonic = "shake128", checkDigest = CheckShake128}
            ]
        }

foo :: NamespaceDefinition
foo =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) "\x11\x7A\x63\xBB\xF0\x4F\x4A\x57\x3C\x6D\x29\xE0\xD0\x32\x4F\x60\x46\xEF\xF3\x14\xB9\xBC\xD9\x65\x87\x2A\x04\x55\x97\x8C\xA2\x1B"
        , mnemonic = "foo"
        , names =
            [ SelfEval{marker = 0x00, mnemonic = "foo"}
            ]
        }

bar :: NamespaceDefinition
bar =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) "\x67\x44\xBA\x37\x14\x8D\x74\x5E\xE0\xB2\x57\x17\xA0\x12\xD1\xE2\x72\x76\x49\xC2\xBD\x28\x51\x98\xA5\x9A\x2D\xEE\x50\x65\x73\x91"
        , mnemonic = "bar"
        , names =
            [ SelfEval{marker = 0x00, mnemonic = "bar"}
            ]
        }

data Foo = Foo Bool Bool Int deriving (Eq, Show)

instance FromBULK Foo where
    parseBULK = withForm (nsName foo "foo") do
        Foo <$> nextBULK <*> nextBULK <*> nextBULK

data Bar = Bar Int Foo deriving (Eq, Show)

instance FromBULK Bar where
    parseBULK = withForm (nsName bar "bar") do
        Bar <$> nextBULK <*> nextBULK

matchTo :: (ToBULK a) => [(a, BULK)] -> IO ()
matchTo = traverse_ (uncurry $ shouldBe . toBULK)
