{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Test.BULK.Decode where

import Control.Exception (ErrorCall, assert, handle)
import Control.Lens
import Data.Bits (Bits (..))
import Data.ByteString.Lazy (ByteString, pack, singleton)
import Data.Digits qualified as D
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Word (Word8)
import System.Random (Random)
import Test.Hspec
import Test.QuickCheck (Gen, Property, arbitrary, choose, forAll, listOf, resize)
import Test.QuickCheck.Instances.ByteString ()
import Prelude hiding (words)

import Data.BULK (BULK (Array, Form, Reference), VersionConstraint (SetVersion), encode, getExpression, getStream, parseLazy, parseTextNotation, toIntegral, _BulkExpr, _Int, _Nat)
import Test.BULK.Encode (bulkNum)

parseStreamWith :: VersionConstraint -> ByteString -> Either String BULK
parseStreamWith version = parseLazy (getStream version)

readFailsOn :: Word8 -> Expectation
readFailsOn word = isLeft (parseLazy getExpression $ singleton word) `shouldBe` True

test_bigger_arrays_decoding :: Int -> Property
test_bigger_arrays_decoding size =
    forAll (arraySizedWith size) $ \array ->
        encode [array] `shouldParseTo` array

parseInts :: (Integral a, Show a) => [(Int, ByteString, a)] -> IO ()
parseInts = traverse_ \(kind, bytes, value) ->
    toIntegral (bulkNum kind bytes) `shouldBe` Just value

shouldParseTo :: ByteString -> BULK -> Expectation
words `shouldParseTo` expr = parseLazy getExpression words `shouldBe` Right expr

shouldParseToPrism :: (Integral a, Bits a, Show a) => Prism' BULK a -> ByteString -> a -> Expectation
shouldParseToPrism prism_ words num = words ^? _BulkExpr . prism_ `shouldBe` Just num

shouldParseToNat :: (Integral a, Bits a, Show a) => ByteString -> a -> Expectation
shouldParseToNat = shouldParseToPrism _Nat

shouldParseToInt :: ByteString -> Int -> Expectation
shouldParseToInt = shouldParseToPrism _Int

shouldDenote :: Text -> [BULK] -> Expectation
text `shouldDenote` list = (parseTextNotation text >>= parseLazy (getStream $ SetVersion 1 0)) `shouldBe` Right (Form list)

unDigits :: [Word8] -> Integer
unDigits = D.unDigits 256 . map fromIntegral

arbitraryByte :: (Num a, Random a) => Gen a
arbitraryByte = choose (0, 255)

assertErrorCall :: IO a -> IO Bool
assertErrorCall action = handle handler $ action $> False
  where
    handler :: ErrorCall -> IO Bool
    handler _ = pure True

-- Arrays

smallInt :: (Num a, Random a) => Gen a
smallInt = choose (0, 63)

smallArray :: Gen [Word8]
smallArray = do
    resize 63 $ listOf arbitraryByte

encodeSmallInt :: Word8 -> ByteString
encodeSmallInt num = assert (num < 64) $ singleton $ 0x80 .|. num

encodeSmallArray :: [Word8] -> ByteString
encodeSmallArray array = assert (len < 64) ((0xC0 .|. len) `cons` pack array)
  where
    len = fromIntegral $ length array

{- | Payload of a random array sized with an integer fitting in N
   | bytes (not a small array)
-}
arraySizedWith :: Int -> Gen BULK
arraySizedWith sizeOrder = do
    size <- choose (minSize, maxSize)
    Array <$> resize size arbitrary
  where
    minSize = if sizeOrder == 1 then 64 else 2 ^ ((sizeOrder - 1) * 8)
    maxSize = 2 ^ (sizeOrder * 8) - 1

-- References

anySimpleMarker :: Gen Word8
anySimpleMarker = choose (0x10, 0x7E)

anySimpleRefBytes :: Gen (Word8, Word8)
anySimpleRefBytes = (,) <$> anySimpleMarker <*> arbitraryByte

anySimpleRef :: Gen BULK
anySimpleRef = do
    (ns, name) <- anySimpleRefBytes
    pure $ Reference (fromIntegral ns) (fromIntegral name)

-- Fixed ranges

reservedMarkers :: [Word8]
reservedMarkers = [0x04 .. 0x0F]

smallWords :: [Word8]
smallWords = [0 .. 63]
