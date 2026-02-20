{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.ByteString.Lazy (ByteString, pack, singleton)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (nubBy)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (chooseInt, elements, forAll, vectorOf)
import Witch (from, via)
import Prelude hiding (readFile)

import Data.BULK
import Data.BULK.BARK qualified as BARK
import Data.BULK.Core qualified as Core
import Data.BULK.Encode (pattern IntReference)
import Data.BULK.Eval (mkContext)
import Test.BULK
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
                    [hex|0102|] `shouldParseTo` Form []
                    [hex|010002|] `shouldParseTo` Form [Nil]
                    [hex|01000100020002|] `shouldParseTo` Form [Nil, Form [Nil], Nil]
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
                            pack [marker, ref] `shouldParseTo` Reference (Ref (via @Int marker) (from ref))
                    prop "reads two-words marker references" $
                        forAll anySimpleRefBytes $ \(marker, ref) ->
                            pack [0x7F, marker, ref] `shouldParseTo` Reference (Ref (from @Int $ 0x7F + fromIntegral marker) (from ref))
                    prop "reads three-words marker references" $
                        forAll anySimpleRefBytes $ \(marker, ref) ->
                            pack [0x7F, 0xFF, marker, ref] `shouldParseTo` Reference (Ref (from @Int $ 0x7F + 0xFF + fromIntegral marker) (from ref))
                it "rejects reserved markers" $
                    traverse_ readFailsOn reservedMarkers
            describe "files" $ do
                it "reads simple files" $ do
                    readFile "test/bulk/nesting.bulk" `shouldReturn` nesting
                    readFile "test/bulk/primitives.bulk" `shouldReturn` primitives
                it "reports bad syntax" $ do
                    readFile "test/bulk/bad nesting.bulk" `shouldReturn` badNesting
                it "checks for version 1.0" $ do
                    readFile "test/bulk/missing version.bulk" `shouldReturn` Left "missing version"
                    readFileV1 "test/bulk/missing version.bulk" `shouldReturnRight` Form [Nil]
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
                encode [Nil, Form [], Array "", IntReference 16 0] `shouldBeRight` [hex|000102C01000|]
            it "encodes natural numbers" $ do
                map @Int encodeNat [0, 1, 0xFF, 0x100, 0xFFFF, 0x1_0000] `shouldBe` [Array "\0", Array "\1", Array "\xFF", Array "\1\0", Array "\xFF\xFF", Array "\0\1\0\0"]
                map @Integer encodeNat [0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF] `shouldBe` [Array [hex|00000000FFFFFFFFFFFFFFFFFFFFFFFF|]]
                encode [Array "\0", Array "\1", Array "\xFF", Array "\1\0"] `shouldBeRight` [hex|8081C1FFC20100|]
            prop "round-trips arbitrary primitives" $ \expr ->
                shouldParseToItself expr
        --
        -- Text Notation
        describe "text notation" $ do
            it "parses notation" $ do
                "nil" `shouldDenote` [Nil]
                "( )" `shouldDenote` [Form []]
                "( nil )" `shouldDenote` [Form [Nil]]
                "( nil ( nil nil ) nil )" `shouldDenote` [Form [Nil, Form [Nil, Nil], Nil]]
                "foo:bar\n( )" `shouldDenote` [IntReference 0x14 0, Form []]
            it "parses small ints" $ for_ smallWords $ \word ->
                [i|w6[#{word}]|] `shouldDenote` [Array $ singleton word]
            it "parses small arrays" $ do
                "#[0]" `shouldDenote` [Array ""]
                "#[1] 0xAB" `shouldDenote` [Array "\xAB"]
                "#[2] 0xCDEF" `shouldDenote` [Array "\xCD\xEF"]
                "#[16] 0xaf5ac1b8-8e33-4025-97fe-f0e2030a00f7" `shouldDenote` [Array [hex|af5ac1b88e33402597fef0e2030a00f7|]]
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
                parseNotation [i|"関数型プログラミング"|] `shouldBeRight` [hex|DEE996A2E695B0E59E8BE38397E383ADE382B0E383A9E3839FE383B3E382B0|]
            it "parses example files" $ do
                parseNotationFile "test/bulk/nesting.bulktext" `shouldReturn` nesting
                parseNotationFile "test/bulk/primitives.bulktext" `shouldReturn` primitives
                parseNotationFile "test/bulk/bad nesting.bulktext" `shouldReturn` badNesting
            it "parses unknown references" $ do
                "foo:bar quux:one foo:baz" `shouldDenote` (uncurry IntReference <$> [(0x14, 0), (0x15, 0), (0x14, 1)])
                "123:one" `shouldDenote` [IntReference 0x14 0]
            it "uses known mnemonics" $ do
                ctx <- loadNotationFiles ctx0 ["test/bulk/123.bulktext"]
                decodeNotationFile @[Int] ctx "test/bulk/321.bulktext" `shouldReturnRight` [1]
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
                pure $ fromBULK (Form $ definitions ++ refs) `shouldBeRight` Form values
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
                decodeNotationFile @[()] ctx0 "test/bulk/123-bad.bulktext" `shouldReturn` Left "test/bulk/123-bad.bulktext: verification failed for namespace (expected digest 0000000000000000000000000000000000000000000000000000000000000000 but got 95e18ecbe701c53459dfc27e816468fe36830996b9b0e9aac7036910a97c8ed7)"
                decodeNotationFile @[()] ctx0 "test/bulk/123-pre.bulktext" `shouldReturn` Left "test/bulk/123-pre.bulktext: verification failed for namespace (missing digest c5c166207da1eecf5d5683ead12647f740a6fe69d51fb3a26719f77b005f0984)"
                decodeNotationFile @[Int] ctx0 "test/bulk/123.bulktext" `shouldReturnRight` [1, 2, 3]
            it "can bootstrap hashing" $ do
                decodeNotationFile @[()] ctx0 "test/bulk/bootstrap-bad.bulktext" `shouldReturn` Left "test/bulk/bootstrap-bad.bulktext: unable to bootstrap namespace: bootstrap"
                decodeNotationFile @[()] ctx0 "config/hash0.bulktext" `shouldReturnRight` []
            it "can bootstrap packages" $ do
                ctx <- loadNotationFiles ctx0 ["test/bulk/config/foo.bulktext", "test/bulk/config/bar.bulktext", "test/bulk/config/foobar.bulktext"]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( package ( 0x16-00 #[4] 0xB4475636 ) 3 ) ) ( bar:bar ( bar:int 2 ) ( bar:foo ( foo:foo true true 99 ) ) )" `shouldBeRight` [Bar 2 (Foo True True 99)]
            it "has verifiable packages" $ do
                decodeNotationFile @[()] ctx0 "test/bulk/package-bad.bulktext" `shouldReturn` Left "test/bulk/package-bad.bulktext: verification failed for package (expected digest 0000000000000000000000000000000000000000000000000000000000000000 but got 7a6dcf4b2cf07e63b60b893c6ac193b55ce38857e18148afc5b113189324747c)"
            it "warns of missing packages" $ do
                ctx <- loadNotationFiles ctx0 ["test/bulk/config/foo.bulktext", "test/bulk/config/bar.bulktext", "test/bulk/config/foobar.bulktext"]
                decodeNotation @[Bar] ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x9DBFD602 ) ) ) ( import 21 ( package ( hash0:shake128 w6[0] ) 2 ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBe` Left [i|not the expected operator: ({21}:0) (expected (bar:bar))\nunknown package: 00\n|]

        --
        -- Parser monad
        describe "Parser monad" $ do
            it "parses Haskell values" $ do
                ctx <- loadNotationFiles ctx0 ["test/bulk/config/foo.bulktext", "test/bulk/config/bar.bulktext", "test/bulk/config/foobar.bulktext"]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x9DBFD602 ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0x37B6D258 ) ) ) ( foo:foo false true 42 )" `shouldBeRight` [Foo False True 42]
                decodeNotation @[Foo] ctx "( import 20 ( namespace ( hash0:shake128 #[4] 0x9DBFD602 ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0x37B6D258 ) ) ) ( foo:foo false true 42 )" `shouldBe` Left "missing version"
                decodeNotation @[Foo] ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x9DBFD602 ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0x37B6D258 ) ) ) ( foo:foo false true nil )" `shouldBe` Left "cannot parse as integer: nil"
                decodeNotation @[Foo] ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x9DBFD602 ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0x37B6D258 ) ) ) ( foo:foo false true )" `shouldBe` Left "no next BULK expression"
                decodeFile ctx "test/bulk/foo-1.bulk" `shouldReturnRight` [Foo False True 42]
                decodeFile ctx "test/bulk/foo-list.bulk" `shouldReturnRight` [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotationFile ctx "test/bulk/foo-list.bulktext" `shouldReturnRight` [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x9DBFD602 ) ) ) ( import 21 ( namespace ( hash0:shake128 #[4] 0x14AE0706 ) ) )  ( import 22 ( namespace ( hash0:shake128 #[4] 0x37B6D258 ) ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBeRight` [Bar 1 (Foo False True 42)]
                decodeNotation ctx "( version 1 0 ) ( import 20 ( namespace ( hash0:shake128 #[4] 0x9DBFD602 ) ) ) ( define ( package ( hash0:shake128 #[4] 0x3C20F61C ) ) ([ nil ( hash0:shake128 #[4] 0x14AE0706 ) ( hash0:shake128 #[4] 0x37B6D258 ) ]) ) ( import 21 2 ( hash0:shake128 #[4] 0x3C20F61C ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBeRight` [Bar 1 (Foo False True 42)]

        --
        -- Custom encoders
        describe "Custom encoders" $ do
            it "encodes simple primitive types" $ do
                matchTo [(True, Core.True), (False, Core.False)]
                matchTo @Int [(1, Array "\1"), (64, Array "\64"), (256, Array "\x01\x00"), (-1, Form [Core.SignedInt, Array "\xFF"])]
                matchTo [([True, False], Form [Core.True, Core.False])]
                matchTo @ByteString [("", Array ""), ("\0\1\2", Array "\0\1\2")]
            it "encodes UTF-8 strings" $ do
                matchTo @Text [("foo", Array "foo"), ("γράφω", Array [hex|CEB3CF81CEACCF86CF89|])]

        --
        -- BARK
        describe "BARK" $ do
            it "reads a manifest" $ do
                ctx <- loadNotationFiles ctx0 ["test/bulk/config/bark-alpha.bulktext"]
                decodeNotationFile ctx "test/bulk/manifest.bulktext" `shouldReturnRight` BARK.BARK [BARK.Description "foo" "0000"]

    describe "slow tests" $ do
        prop "reads really big generic arrays" $ test_bigger_arrays_decoding 3
        prop "has self-evaluating expressions" $ forAll (vectorOf 8 simpleBULK) $ \exprs ->
            fromBULK (Form exprs) `shouldBeRight` Form exprs

nesting, primitives, badNesting :: Either String BULK
nesting = Right $ Form [version 1 0, Form [], Form [Nil, Form [Nil], Form []]]
primitives =
    Right $
        Form
            [ version 1 0
            , Form
                [ Nil
                , Array "Hello world!"
                , Array [hex|2A|]
                , Array ""
                , Array [hex|40|]
                , Array [hex|0100|]
                , Array [hex|01000000|]
                , Array [hex|0123456789ABCDEF|]
                , IntReference 0x14 0x01
                , IntReference 0x14 0x02
                , IntReference 0x7E 0xFF
                , IntReference (0x7F + 0xFF + 0xBC) 0x1A
                ]
            ]
badNesting = Left "not enough data (while reading a form)"

parseOnlyIntCases, bidirectionalIntCases :: [(BULK, ByteString, Int)]
parseOnlyIntCases =
    [ (Core.UnsignedInt, [hex|01|], 0x1)
    , (Core.SignedInt, [hex|0001|], 0x1)
    , (Core.SignedInt, [hex|FFFF|], -0x1)
    , (Core.SignedInt, [hex|00000001|], 0x1)
    , (Core.SignedInt, [hex|FFFFFFFF|], -0x1)
    , (Core.SignedInt, [hex|0000000000000001|], 0x1)
    , (Core.SignedInt, [hex|FFFFFFFFFFFFFFFF|], -0x1)
    ]
bidirectionalIntCases =
    [ (Core.SignedInt, [hex|01|], 0x1)
    , (Core.SignedInt, [hex|7F|], 0x7F)
    , (Core.SignedInt, [hex|FF|], -0x1)
    , (Core.SignedInt, [hex|80|], -0x80)
    , (Core.SignedInt, [hex|81|], -0x7F)
    , (Core.SignedInt, [hex|7FFF|], 0x7FFF)
    , (Core.SignedInt, [hex|8000|], -0x8000)
    , (Core.SignedInt, [hex|8001|], -0x7FFF)
    , (Core.SignedInt, [hex|7FFFFFFF|], 0x7FFF_FFFF)
    , (Core.SignedInt, [hex|80000000|], -0x8000_0000)
    , (Core.SignedInt, [hex|80000001|], -0x7FFF_FFFF)
    , (Core.SignedInt, [hex|7FFFFFFFFFFFFFFF|], 0x7FFF_FFFF_FFFF_FFFF)
    , (Core.SignedInt, [hex|8000000000000000|], -0x8000_0000_0000_0000)
    , (Core.SignedInt, [hex|8000000000000001|], -0x7FFF_FFFF_FFFF_FFFF)
    ]

bigIntCases :: [(BULK, ByteString, Integer)]
bigIntCases =
    [ (Core.SignedInt, [hex|80000000000000000000000000000000|], -0x8000_0000_0000_0000_0000_0000_0000_0000)
    , (Core.SignedInt, [hex|80000000000000000000000000000001|], -0x7FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF)
    , (Core.SignedInt, [hex|8000000000000000000000000000000000000000000000000000000000000000|], -0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000)
    , (Core.SignedInt, [hex|8000000000000000000000000000000000000000000000000000000000000001|], -0x7FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF)
    ]

hash0 :: Namespace
hash0 =
    Namespace
        { matchID = MatchNamePrefix 0x00 [hex|9DBFD6029C1EBE32EC16749703A283DFC1B47C4E925473435529B5769FD89311|]
        , mnemonic = "hash0"
        , names = [Name 0x00 (Just "shake128") $ Digest CheckShake128]
        }

shake128 :: Ref
shake128 = Ref hash0.matchID $ head hash0.names

ctx0 :: Context
ctx0 = mkContext [hash0]

foo :: Namespace
foo =
    Namespace
        { matchID = MatchQualifiedNamePrefix shake128 [hex|37B6D2582C3A962E2CDB2BF89C47D17179D6F1A3425E73A32010C0CA32AC55BA|]
        , mnemonic = "foo"
        , names = []
        }

bar :: Namespace
bar =
    Namespace
        { matchID = MatchQualifiedNamePrefix shake128 [hex|14AE0706F60122731B16D0D5A882C5ACEC93C907C5A84EB65ECB3A5E1167BBEB|]
        , mnemonic = "bar"
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

matchTo :: (HasCallStack, ToBULK a) => [(a, BULK)] -> IO ()
matchTo = traverse_ (uncurry $ shouldBe . toBULK)
