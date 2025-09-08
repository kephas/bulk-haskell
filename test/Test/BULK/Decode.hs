module Test.BULK.Decode where

import Control.Exception (ErrorCall, handle)
import Data.ByteString.Lazy (ByteString, pack)
import Data.Digits qualified as D
import Data.Either (isLeft)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Word (Word8)
import System.Random (Random)
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, choose, listOf, resize)
import Test.QuickCheck.Instances.ByteString ()
import Prelude hiding (words)

import Data.BULK (BULK (Array, Form, Reference), VersionConstraint (SetVersion), getExpression, getStream, parseLazy, parseTextNotation, toIntegral)

readBin :: [Word8] -> Either String BULK
readBin = parseLazy getExpression . pack

readBinStream :: VersionConstraint -> [Word8] -> Either String BULK
readBinStream version = parseLazy (getStream version) . pack

readFails :: [Word8] -> Expectation
readFails words = isLeft (readBin words) `shouldBe` True

shouldParseTo :: ByteString -> BULK -> Expectation
words `shouldParseTo` expr = parseLazy getExpression words `shouldBe` Right expr

shouldDenote :: Text -> [BULK] -> Expectation
text `shouldDenote` list = (parseTextNotation text >>= parseLazy (getStream $ SetVersion 1 0)) `shouldBe` Right (Form list)

readNum :: (Integral a) => [Word8] -> Either String a
readNum words =
    readBin words >>= maybeToEither "not a number" . toIntegral

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither nothing = maybe (Left nothing) Right

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

lengthAsWord :: [a] -> Word8
lengthAsWord = fromIntegral . length

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
