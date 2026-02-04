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
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, chooseInt, forAll, listOf)
import Text.Hex qualified as H
import Witch (from, via)
import Prelude hiding (readFile)

import Data.BULK
import Data.BULK.Encode (pattern IntReference)
import Data.BULK.Eval (mkContext)
import Data.BULK.Types (pattern Core)
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
                "version" `shouldDenote` [IntReference 16 0]
                "( version 1 0 ) true false ( define 0x1400 ( subst ( concat ( arg 1 ) ( arg 2 ) ) ) )" `shouldDenote` [version 1 0, Core 1, Core 2, Form [Core 6, IntReference 0x14 0, Form [Core 0x0B, Form [Core 0x0A, Form [Core 0x0C, Array "\1"], Form [Core 0x0C, Array "\2"]]]]]
                "( bulk:ns 0x1400 #[4] 0x0011-2233 ) ( bulk:ns-mnemonic 0x1400 )" `shouldDenote` [Form [Core 3, IntReference 0x14 0, Array "\x00\x11\x22\x33"], Form [Core 8, IntReference 0x14 0]]
            it "parses UTF-8 strings" $ do
                parseNotation [i|"foo"|] `shouldBe` Right "\xC3\&foo"
                parseNotation [i|"foo" "quuux"|] `shouldBe` Right "\xC3\&foo\xC5quuux"
                parseNotation [i|"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"|] `shouldBe` Right "\x03\xC1\78Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
                parseNotation [i|"関数型プログラミング"|] `shouldBe` Right "\xDE\233\150\162\230\149\176\229\158\139\227\131\151\227\131\173\227\130\176\227\131\169\227\131\159\227\131\179\227\130\176"
            it "parses example files" $ do
                parseNotationFile "test/nesting.bulktext" `shouldReturn` nesting
                parseNotationFile "test/primitives.bulktext" `shouldReturn` primitives
                parseNotationFile "test/bad nesting.bulktext" `shouldReturn` badNesting
            it "parses unknown references" $ do
                "foo:bar quux:one foo:baz" `shouldDenote` (uncurry IntReference <$> [(0x14, 0), (0x15, 0), (0x14, 1)])
                "123:one" `shouldDenote` [IntReference 0x14 0]
            it "uses known mnemonics" $ do
                ctx <- loadNotationFiles ctx0 ["test/123.bulktext"]
                decodeNotationFile @[Int] ctx "test/321.bulktext" `shouldReturn` Right [1]
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
                pure $ eval' [] (Form $ definitions ++ refs) `shouldBe` Right (Form values)
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
                    encodeInt @Int (-1) `shouldBe` Form [Core 0x21, Array "\xFF"]
                    for_ bidirectionalIntCases \(kind, bytes, value) ->
                        encodeInt value `shouldBe` Form [Core kind, Array bytes]
            it "has verifiable namespaces" $ do
                decodeNotationFile @[()] ctx0 "test/123-bad.bulktext" `shouldReturn` Left "verification failed for namespace: 123 (expected digest 00000000000000000000000000000000 but got dd3bff1608fa25cc16ba90c0f8b4976e4a50b1d215cf8448e890e7cc4a4b0ff0)"
                decodeNotationFile @[()] ctx0 "test/123-pre.bulktext" `shouldReturn` Left "verification failed for namespace: 123 (missing digest f8597e4ccb42898c22c4f0eb0f11e73374020b312369547f4e85009d1ff152ca)"
                decodeNotationFile @[Int] ctx0 "test/123.bulktext" `shouldReturn` Right [1, 2, 3]
            it "can bootstrap hashing" $ do
                decodeNotationFile @[()] ctx0 "test/bootstrap-bad.bulktext" `shouldReturn` Left "unable to bootstrap namespace: bootstrap"
                decodeNotationFile @[()] ctx0 "config/hash0.bulktext" `shouldReturn` Right []
            it "has verifiable packages" $ do
                decodeNotationFile @[()] ctx0 "test/package-bad.bulktext" `shouldReturn` Left "verification failed for package (expected digest 00000000000000000000000000000000 but got 7a6dcf4b2cf07e63b60b893c6ac193b55ce38857e18148afc5b113189324747c)"
            it "has lasting namespaces and packages" $ do
                ctx <- loadNotationFiles ctx0 ["test/config/foo.bulktext", "test/config/bar.bulktext", "test/config/foobar.bulktext"]
                decodeNotation ctx "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( import 21 2 ( hash0:shake128 #[4] 0x936AFC0C ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBe` Right [Bar 1 (Foo False True 42)]

        --
        -- Parser monad
        describe "Parser monad" $ do
            it "parses Haskell values" $ do
                ctx <- loadNotationFiles ctx0 ["test/config/foo.bulktext", "test/config/bar.bulktext", "test/config/foobar.bulktext"]
                decodeNotation ctx "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true 42 )" `shouldBe` Right [Foo False True 42]
                decodeNotation @[Foo] ctx "( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true 42 )" `shouldBe` Left "missing version"
                decodeNotation @[Foo] ctx "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true nil )" `shouldBe` Left "cannot parse as integer: nil"
                decodeNotation @[Foo] ctx "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x117A63BB ) ) ( foo:foo false true )" `shouldBe` Left "no next BULK expression"
                decodeFile ctx "test/foo.bulk" `shouldReturn` Right [Foo False True 42]
                decodeFile ctx "test/foos.bulk" `shouldReturn` Right [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotationFile ctx "test/foos.bulktext" `shouldReturn` Right [Foo True True 1, Foo True False 1, Foo False True 2, Foo False False 3, Foo True True 5, Foo False False 8]
                decodeNotation ctx "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( ns 21 ( hash0:shake128 #[4] 0x7F28DB08 ) )  ( ns 22 ( hash0:shake128 #[4] 0x117A63BB ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBe` Right [Bar 1 (Foo False True 42)]
                decodeNotation ctx "( version 1 0 ) ( ns 20 ( hash0:shake128 #[4] 0xE2ECDA49 ) ) ( package ( hash0:shake128 #[4] 0xDBE86354 ) nil ( hash0:shake128 #[4] 0x7F28DB08 ) ( hash0:shake128 #[4] 0x117A63BB ) ) ( import 21 2 ( hash0:shake128 #[4] 0xDBE86354 ) ) ( bar:bar ( bar:int 1 ) ( bar:foo ( foo:foo false true 42 ) ) )" `shouldBe` Right [Bar 1 (Foo False True 42)]

        --
        -- Custom encoders
        describe "Custom encoders" $ do
            it "encodes simple primitive types" $ do
                matchTo [(True, Core 0x01), (False, Core 0x02)]
                matchTo @Int [(1, Array "\1"), (64, Array "\64"), (256, Array "\x01\x00"), (-1, Form [Core 0x21, Array "\xFF"])]
                matchTo [([True, False], Form [Core 0x01, Core 0x02])]
                matchTo @ByteString [("", Array ""), ("\0\1\2", Array "\0\1\2")]
            it "encodes UTF-8 strings" $ do
                matchTo @Text [("foo", Array "foo"), ("γράφω", Array $ fromHex "CEB3CF81CEACCF86CF89")]

    describe "slow tests" $ do
        prop "reads really big generic arrays" $ test_bigger_arrays_decoding 3
        prop "has self-evaluating expressions" $ forAll (listOf simpleBULK) $ \exprs ->
            eval' [] (Form exprs) `shouldBe` Right (Form exprs)

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
        { matchID = MatchNamePrefix 0x00 $ fromHex "E2ECDA494A78195B07034AB10B2C4390C94C01251B13D1A483D21E8B781D703D"
        , mnemonic = "hash0"
        , names =
            [ DigestName{marker = 0x00, mnemonic = "shake128", checkDigest = CheckShake128}
            ]
        }

ctx0 :: Context
ctx0 = mkContext [hash0]

foo :: NamespaceDefinition
foo =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "117A63BBF04F4A573C6D29E0D0324F6046EFF314B9BCD965872A0455978CA21B"
        , mnemonic = "foo"
        , names = []
        }

bar :: NamespaceDefinition
bar =
    NamespaceDefinition
        { matchID = MatchQualifiedNamePrefix (Name (AssociatedNamespace hash0) 0x00) $ fromHex "7F28DB08DE2CAA8D3D88309A40484C5EFB05CC2E2B8F5C14602692FFDE7D2CB6"
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

eval' :: [NamespaceDefinition] -> BULK -> Either String BULK
eval' nss = eval (mkContext nss)

matchTo :: (ToBULK a) => [(a, BULK)] -> IO ()
matchTo = traverse_ (uncurry $ shouldBe . toBULK)

fromHex :: Text -> ByteString
fromHex = fromStrict . fromJust . H.decodeHex
