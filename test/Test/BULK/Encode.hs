{-# OPTIONS_GHC -Wno-orphans #-}

module Test.BULK.Encode where

import Data.BULK (BULK (..))
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, oneof, sized)
import Test.QuickCheck.Instances.ByteString ()
import Prelude hiding (words)

instance Arbitrary BULK where
    arbitrary = oneof [pure Nil, Form <$> sized form, Array <$> arbitrary, ref]
      where
        form :: Int -> Gen [BULK]
        form 0 = pure []
        form n = do
            headSize <- chooseInt (1, n)
            let restSize = n - headSize
            (:) <$> (Form <$> form (headSize - 1)) <*> form restSize
        ref = Reference <$> chooseInt (0x10, 0xFFFF) <*> chooseInt (0x00, 0xFF)
    shrink Nil = []
    shrink (Form exprs) = Form <$> shrink exprs
    shrink (Array bs) = Array <$> shrink bs
    shrink (Reference ns name) = Reference <$> shrink ns <*> shrink name
