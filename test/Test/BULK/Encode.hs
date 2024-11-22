{-# OPTIONS_GHC -Wno-orphans #-}

module Test.BULK.Encode where

import Data.BULK (BULK (..))
import Data.ByteString.Lazy (ByteString)
import Test.QuickCheck (Arbitrary (..), Gen, choose, chooseInt, oneof, resize, sized)
import Test.QuickCheck.Instances.ByteString ()
import Prelude hiding (words)

instance Arbitrary BULK where
    arbitrary = oneof [pure Nil, Form <$> sized form, Array <$> smallBS]
      where
        form :: Int -> Gen [BULK]
        form 0 = pure []
        form n = do
            headSize <- chooseInt (1, n)
            let restSize = n - headSize
            (:) <$> (Form <$> form (headSize - 1)) <*> form restSize

smallBS :: Gen ByteString
smallBS = do
    size <- choose (0, 63)
    resize size arbitrary
