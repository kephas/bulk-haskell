{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Instances.BULK where

import Data.BULK (BULK (..))
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, frequency, getSize, resize, sized)
import Test.QuickCheck.Instances.ByteString ()
import Prelude hiding (words)

instance Arbitrary BULK where
    arbitrary = frequency [(1, nil), (32, simpleForm), (16, biggerForm), (2, form), (4, array), (8, ref)]
    shrink Nil = []
    shrink (Form exprs) = Form <$> shrink exprs
    shrink (Array bs) = Array <$> shrink bs
    shrink (Reference ns name) = Reference <$> shrink ns <*> shrink name

nil, array, ref, simpleForm, biggerForm, form :: Gen BULK
nil = pure Nil
array = Array <$> arbitrary
ref = Reference <$> frequency [(4, chooseInt (0x10, 0x17)), (16, chooseInt (0x18, 0x7F)), (1, chooseInt (0x80, 0xFFFF))] <*> chooseInt (0x00, 0xFF)
simpleForm = do
    operator <- ref
    operand <- arbitrary
    pure $ Form [operator, operand]
biggerForm = do
    operator <- ref
    size <- getSize
    operands <- list size
    pure $ Form $ operator : operands
form = Form <$> sized list

list :: Int -> Gen [BULK]
list 0 = pure []
list n = do
    headSize <- chooseInt (1, n)
    let restSize = n - headSize
    (:) <$> resize (headSize - 1) arbitrary <*> list restSize
