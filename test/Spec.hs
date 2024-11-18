{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Arrays
import Data.BULK
import Data.Word (Word8)
import Numbers
import References
import Test.Hspec
import Utils
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
            test_arrays_decoding
            test_number_decoding
            test_references
            it "rejects reserved markers" $
                mapM_ (\marker -> readFails [marker, 0, 0, 0]) reservedMarkers
        describe "files" $ do
            it "reads simple files" $ do
                readFile "test/nesting.bulk"
                    `shouldReturn` Right
                        ( Form
                            [ Form [Reference 16 0, Array [1], Array [0]]
                            , Form []
                            , Form [Nil, Form [Nil], Form []]
                            ]
                        )

                readFile "test/primitives.bulk"
                    `shouldReturn` Right
                        ( Form
                            [ Form [Reference 16 0, Array [1], Array [0]]
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
            encode [Nil] `shouldBe` [0]

reservedMarkers :: [Word8]
reservedMarkers = [0x04 .. 0x0F]
