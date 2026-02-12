{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Data.ByteString.Lazy (ByteString, fromStrict, pack, singleton)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (chooseInt, elements, forAll, vectorOf)
import Text.Hex qualified as H
import Witch (from, via)
import Prelude hiding (readFile)

import Data.BULK
import Data.BULK.BARK qualified as BARK
import Data.BULK.Core qualified as Core
import Data.BULK.Encode (pattern IntReference)
import Data.BULK.Eval (mkContext)
import Test.BULK.Decode
import Test.BULK.Encode ()
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
                    readFileV1 "test/missing version.bulk" `shouldReturnRight` Form [Nil]
            describe "version and profile" $ do
                it "checks for version 1.0" $ do
                    parseStream "\x01\x10\x00\x81\x80\x02" `shouldBeRight` Form [Form [IntReference 16 0, 1, 0]]
                    parseStream "\x01\x10\x00\x81\x82\x02" `shouldBeRight` Form [Form [IntReference 16 0, 1, 2]]
                    parseStream "\x01\x10\x00\x82\x80\x02" `shouldBe` Left "this application only supports BULK version 1.x"
                    parseStream "\x01\x10\x00\x81\x00\x00\x02" `shouldBe` Left "malformed version"
                    parseStream "\0" `shouldBe` Left "missing version"
                    parseStreamV1 "\0" `shouldBeRight` Form [Nil]
        --
        -- Encoding
        describe "encoding" $ do
            it "encodes primitives" $ do
                encode [Nil, Form [], Array "", IntReference 16 0] `shouldBe` "\x00\x01\x02\xC0\x10\x00"
            it "encodes natural numbers" $ do
                map @Int encodeNat [0, 1, 0xFF, 0x100, 0xFFFF, 0x1_0000] `shouldBe` [Array "\0", Array "\1", Array "\xFF", Array "\1\0", Array "\xFF\xFF", Array "\0\1\0\0"]
                map @Integer encodeNat [0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF] `shouldBe` [Array $ fromHex "00000000FFFFFFFFFFFFFFFFFFFFFFFF"]
                encode [Array "\0", Array "\1", Array "\xFF", Array "\1\0"] `shouldBe` fromHex "8081C1FFC20100"
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
                "#[16] 0xaf5ac1b8-8e33-4025-97fe-f0e2030a00f7" `shouldDenote` [Array $ fromHex "af5ac1b88e33402597fef0e2030a00f7"]
            it "parses small decimals" $ do
                for_ smallWords $ \word ->
                    [i|#{word}|] `shouldDenote` [Array $ singleton word]
            it "parses arrays" $ do
                "# 64 0x0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000-0000000000000000" `shouldDenote` [Array $ pack $ replicate 64 0]
            prop "parses bigger decimals" $ forAll (chooseInt (64, maxBound)) $ \num ->
                [i|#{num}|] `shouldDenote` [encodeNat num]
            it "parses bulk core references" $ do
                "version" `shouldDenote` [Core.Version]
                "( version 1 0 ) true false ( import ( namespace ) ) ( import ( package ) )" `shouldDenote` [version 1 0, Core.True, Core.False, Form [Core.Import, Form [Core.Namespace]], Form [Core.Import, Form [Core.Package]]]
                "( bulk:namespace 0x1400 #[4] 0x0011-2233 ) ( bulk:package 0x1400 )" `shouldDenote` [Form [Core.Namespace, IntReference 0x14 0, Array "\x00\x11\x22\x33"], Form [Core.Package, IntReference 0x14 0]]
            it "parses UTF-8 strings" $ do
                parseNotation [i|"foo"|] `shouldBeRight` "\xC3\&foo"
                parseNotation [i|"foo" "quuux"|] `shouldBeRight` "\xC3\&foo\xC5quuux"
                parseNotation [i|"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"|] `shouldBeRight` "\x03\xC1\78Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
                parseNotation [i|"関数型プログラミング"|] `shouldBeRight` "\xDE\233\150\162\230\149\176\229\158\139\227\131\151\227\131\173\227\130\176\227\131\169\227\131\159\227\131\179\227\130\176"
            it "parses example files" $ do
                parseNotationFile "test/nesting.bulktext" `shouldReturn` nesting
                parseNotationFile "test/primitives.bulktext" `shouldReturn` primitives
                parseNotationFile "test/bad nesting.bulktext" `shouldReturn` badNesting
            it "parses unknown references" $ do
                "foo:bar quux:one foo:baz" `shouldDenote` (uncurry IntReference <$> [(0x14, 0), (0x15, 0), (0x14, 1)])
                "123:one" `shouldDenote` [IntReference 0x14 0]
            it "uses known mnemonics" $ do
                ctx <- loadNotationFiles ctx0 ["test/123.bulktext"]
                decodeNotationFile @[Int] ctx "test/321.bulktext" `shouldReturnRight` [1]
            it "parses nested BULK" $ do
                "([ nil ])" `shouldDenote` [Array "\0"]
                "nil ([ nil ]) nil" `shouldDenote` [Nil, Array "\0", Nil]
                "([ ( nil ( ) nil ) nil ]) nil" `shouldDenote` [Array "\1\0\1\2\0\2\0", Nil]
        --
        -- Core namespace and evaluation
        describe "core namespace" $ do
            prop "has definitions" $ do
                rvs <- nubBy ((==) `on` fst) <$> vectorOf 8 ((,) <$> anySimpleRef <*> elements [Core.True, Core.False])
                let refs = map fst rvs
                    values = map snd rvs
                    definitions = zipWith define refs values
                pure $ eval' (Form $ definitions ++ refs) `shouldBeRight` Form values
            it "respect scoping rule" do
                shouldFail $ decodeNotation @[[Int]] (mkContext []) "( ( bulk:define 0x1400 42 ) 0x1400 ) ( 0x1400 )"
            describe "parses numbers" $ do
                it "small ints" $ for_ smallWords \w ->
                    encodeSmallInt w `shouldParseToInt` fromIntegral w
                it "typed Int forms" $ do
                    parseInts $ parseOnlyIntCases ++ bidirectionalIntCases
                    parseInts bigIntCases
            describe "encodes numbers" $ do
                it "ints" $ do
                    encodeInt @Int (-1) `shouldBe` Form [Core.SignedInt, Array "\xFF"]
                    for_ bidirectionalIntCases \(kind, bytes, value) ->
                        encodeInt value `shouldBe` Form [kind, Array bytes]
            it "has verifiable namespaces" $ do
                decodeNotationFile @[()] ctx0 "test/123-bad.bulktext" `shouldReturn` Left "verification failed for namespace (expected digest 0000000000000000000000000000000000000000000000000000000000000000 but got 95e18ecbe701c53459dfc27e816468fe36830996b9b0e9aac7036910a97c8ed7)"
                decodeNotationFile @[()] ctx0 "test/123-pre.bulktext" `shouldReturn` Left "verification failed for namespace (missing digest bf1e7a30c96b6db78166186215278ffe37e83640f32f0333ccb6589649782c1f)"
                decodeNotationFile @[Int] ctx0 "test/123.bulktext" `shouldReturnRight` [1, 2, 3]
            it "can bootstrap hashing" $ do
                decodeNotationFile @[()] ctx0 "test/bootstrap-bad.bulktext" `shouldReturn` Left "unable to bootstrap namespace: bootstrap"
                decodeNotationFile @[()] ctx0 "config/hash0.bulktext" `shouldReturnRight` []
            it "has verifiable packages" $ do
                decodeNotationFile @[()] ctx0 "test/package-bad.bulktext" `shouldReturn` Left "verification failed for package (expected digest 0000000000000000000000000000000000000000000000000000000000000000 but got 7a6dcf4b2cf07e63b60b893c6ac193b55ce38857e18148afc5b113189324747c)"
            it "has lasting namespaces and packages" $ do
                ctx <- loadNotationFiles ctx0 ["test/config/foo.bulktext", "test/config/bar.bulktext", "test/config/foobar.bulktext"]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( import 21 ( package ( hash0:shake128 #[4] 0xE636DA00 ) 2 ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBeRight` [Bar 1 (Foo False True 42)]
            it "has new syntax" $ do
                ctx <- loadNotationFiles ctx0 ["test/config/foo.bulktext", "test/config/bar.bulktext", "test/config/foobar.bulktext"]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0xEDA7D7C1 ) ) ) ( foo:foo false true 42 )" `shouldBeRight` [Foo False True 42]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( define ( package ( hash0:shake128 #[4] 0xDD8AD9F4 ) ) ([ nil ( hash0:shake128 #[4] 0xBF5102C6 ) ( hash0:shake128 #[4] 0xEDA7D7C1 ) ]) ) ( import 21 ( package ( hash0:shake128 #[4] 0xDD8AD9F4 ) 2 ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBeRight` [Bar 1 (Foo False True 42)]
                decodeNotation @[Int] ctx0 "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( define ( namespace ( hash0:shake128 #[4] 0xF1D12CD2 ) 21 ) ([ nil ( define quux:quux 0 ) ]) ) quux:quux" `shouldBeRight` [0]
                decodeNotation @[Quux] ctx0 [i|( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 \#[4] 0x99FE9CBE ) ) ) ( define ( namespace ( hash0:shake128 \#[4] 0x03A5F567 ) 21 ) ([ nil ( mnemonic ( namespace 21 ) "quux" ) ( mnemonic quux:quuux "quuux" ) ]) ) ( 0x1500 )|] `shouldBeRight` [Quux]

        --
        -- Parser monad
        describe "Parser monad" $ do
            it "parses Haskell values" $ do
                ctx <- loadNotationFiles ctx0 ["test/config/foo.bulktext", "test/config/bar.bulktext", "test/config/foobar.bulktext"]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0xEDA7D7C1 ) ) ) ( foo:foo false true 42 )" `shouldBeRight` [Foo False True 42]
                decodeNotation @[Foo] ctx "( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0xEDA7D7C1 ) ) ) ( foo:foo false true 42 )" `shouldBe` Left "missing version"
                decodeNotation @[Foo] ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0xEDA7D7C1 ) ) ) ( foo:foo false true nil )" `shouldBe` Left "cannot parse as integer: nil"
                decodeNotation @[Foo] ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0xEDA7D7C1 ) ) ) ( foo:foo false true )" `shouldBe` Left "no next BULK expression"
                decodeFile ctx "test/foo.bulk" `shouldReturnRight` [Foo False True 42]
                decodeFile ctx "test/foos.bulk" `shouldReturnRight` [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotationFile ctx "test/foos.bulktext" `shouldReturnRight` [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0xBF5102C6 ) ) )  ( import 22 ( namespace ( hash0:shake128 #[4] 0xEDA7D7C1 ) ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBeRight` [Bar 1 (Foo False True 42)]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x99FE9CBE ) ) ) ( define ( package ( hash0:shake128 #[4] 0xDD8AD9F4 ) ) ([ nil ( hash0:shake128 #[4] 0xBF5102C6 ) ( hash0:shake128 #[4] 0xEDA7D7C1 ) ]) ) ( import 21 2 ( hash0:shake128 #[4] 0xDD8AD9F4 ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBeRight` [Bar 1 (Foo False True 42)]

        --
        -- Custom encoders
        describe "Custom encoders" $ do
            it "encodes simple primitive types" $ do
                matchTo [(True, Core.True), (False, Core.False)]
                matchTo @Int [(1, Array "\1"), (64, Array "\64"), (256, Array "\x01\x00"), (-1, Form [Core.SignedInt, Array "\xFF"])]
                matchTo [([True, False], Form [Core.True, Core.False])]
                matchTo @ByteString [("", Array ""), ("\0\1\2", Array "\0\1\2")]
            it "encodes UTF-8 strings" $ do
                matchTo @Text [("foo", Array "foo"), ("γράφω", Array $ fromHex "CEB3CF81CEACCF86CF89")]

        --
        -- BARK
        describe "BARK" $ do
            it "reads a manifest" $ do
                ctx <- loadNotationFiles ctx0 ["test/config/bark-alpha.bulktext"]
                decodeNotationFile ctx "test/manifest.bulktext" `shouldReturnRight` [BARK.Description "foo" "0000"]

    describe "slow tests" $ do
        prop "reads really big generic arrays" $ test_bigger_arrays_decoding 3
        prop "has self-evaluating expressions" $ forAll (vectorOf 8 simpleBULK) $ \exprs ->
            eval' (Form exprs) `shouldBeRight` Form exprs

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

parseOnlyIntCases, bidirectionalIntCases :: [(BULK, ByteString, Int)]
parseOnlyIntCases =
    [ (Core.UnsignedInt, "\x01", 0x1)
    , (Core.SignedInt, "\x00\x01", 0x1)
    , (Core.SignedInt, "\xFF\xFF", -0x1)
    , (Core.SignedInt, "\x00\x00\x00\x01", 0x1)
    , (Core.SignedInt, "\xFF\xFF\xFF\xFF", -0x1)
    , (Core.SignedInt, "\x00\x00\x00\x00\x00\x00\x00\x01", 0x1)
    , (Core.SignedInt, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", -0x1)
    ]
bidirectionalIntCases =
    [ (Core.SignedInt, "\x01", 0x1)
    , (Core.SignedInt, "\x7F", 0x7F)
    , (Core.SignedInt, "\xFF", -0x1)
    , (Core.SignedInt, "\x80", -0x80)
    , (Core.SignedInt, "\x81", -0x7F)
    , (Core.SignedInt, "\x7F\xFF", 0x7FFF)
    , (Core.SignedInt, "\x80\x00", -0x8000)
    , (Core.SignedInt, "\x80\x01", -0x7FFF)
    , (Core.SignedInt, "\x7F\xFF\xFF\xFF", 0x7FFF_FFFF)
    , (Core.SignedInt, "\x80\x00\x00\x00", -0x8000_0000)
    , (Core.SignedInt, "\x80\x00\x00\x01", -0x7FFF_FFFF)
    , (Core.SignedInt, "\x7F\xFF\xFF\xFF\xFF\xFF\xFF\xFF", 0x7FFF_FFFF_FFFF_FFFF)
    , (Core.SignedInt, "\x80\x00\x00\x00\x00\x00\x00\x00", -0x8000_0000_0000_0000)
    , (Core.SignedInt, "\x80\x00\x00\x00\x00\x00\x00\x01", -0x7FFF_FFFF_FFFF_FFFF)
    ]

bigIntCases :: [(BULK, ByteString, Integer)]
bigIntCases =
    [(Core.SignedInt, "\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", -0x8000_0000_0000_0000_0000_0000_0000_0000)]

hash0 :: NamespaceDefinition
hash0 =
    NamespaceDefinition
        { matchID = MatchNamePrefix 0x00 $ fromHex "99FE9CBED1B3F0D34869530AA1E6A8AE699C8954714A29696DA4386AC7B7B487"
        , mnemonic = "hash0"
        , names = [NameDefinition 0x00 "shake128" $ Digest CheckShake128]
        }

ctx0 :: Context
ctx0 = mkContext [hash0]

foo :: NamespaceDefinition
foo =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "EDA7D7C1A569382D25212D9CDD1595939A4B6D662879D5B33C2DC6DD5F7F1230"
        , mnemonic = "foo"
        , names = []
        }

bar :: NamespaceDefinition
bar =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "BF5102C66BDC98D2C60280E321D5BD28A87D15A6B491767F6F1D0E0ECD009AF4"
        , mnemonic = "bar"
        , names = []
        }

quux :: NamespaceDefinition
quux =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "03A5F567"
        , mnemonic = "quux"
        , names = []
        }

data Foo = Foo Bool Bool Int deriving (Eq, Show)

instance FromBULK Foo where
    parseBULK = foo <*:> "foo" $ do
        Foo <$> nextBULK <*> nextBULK <*> nextBULK

data Bar = Bar Int Foo deriving (Eq, Show)

instance FromBULK Bar where
    parseBULK = bar <*:> "bar" $ do
        Bar <$> (bar <:> "int") nextBULK <*> (bar <:> "foo") nextBULK

data Quux = Quux deriving (Eq, Show)

instance FromBULK Quux where
    parseBULK = quux <*:> "quuux" $ pure Quux

eval' :: BULK -> Either String BULK
eval' = eval (mkContext [])

matchTo :: (HasCallStack, ToBULK a) => [(a, BULK)] -> IO ()
matchTo = traverse_ (uncurry $ shouldBe . toBULK)

fromHex :: Text -> ByteString
fromHex = fromStrict . fromJust . H.decodeHex
