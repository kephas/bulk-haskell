{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Instances.BULK where

import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, frequency, getSize, resize, sized)
import Test.QuickCheck.Instances.ByteString ()
import Witch.From (from)
import Prelude hiding (words)

import Data.BULK (BULK (..), Name (..), Ref (..), Value (..))
import Data.BULK.Types (NamespaceID)

instance Arbitrary BULK where
    arbitrary = bulk arbitrary
    shrink Nil = []
    shrink (Form exprs) = Form <$> shrink exprs
    shrink (Array bs) = Array <$> shrink bs
    shrink (Reference (Ref ns name)) = do
        newRef <- Ref <$> shrink ns <*> shrink name
        pure $ Reference newRef

instance Arbitrary NamespaceID where
    arbitrary = from <$> frequency [(4, chooseInt (0x10, 0x13)), (16, chooseInt (0x14, 0x7F)), (1, chooseInt (0x80, 0xFFFF))]

instance Arbitrary Name where
    arbitrary = Name <$> arbitrary <*> pure Nothing <*> pure SelfEval

simpleNS :: Gen NamespaceID
simpleNS = from <$> chooseInt (0x14, 0x7F)

simpleBULK :: Gen BULK
simpleBULK = bulk simpleNS

nil, array :: Gen BULK
nil = pure Nil
array = Array <$> arbitrary

bulk, ref, simpleForm, biggerForm, form :: Gen NamespaceID -> Gen BULK
bulk ns = frequency [(1, nil), (32, simpleForm ns), (16, biggerForm ns), (2, form ns), (4, array), (8, ref ns)]
ref ns = Reference <$> (Ref <$> ns <*> arbitrary)
simpleForm ns = do
    operator <- ref ns
    operand <- bulk ns
    pure $ Form [operator, operand]
biggerForm ns = do
    operator <- ref ns
    size <- getSize
    operands <- list ns size
    pure $ Form $ operator : operands
form ns = Form <$> sized (list ns)

list :: Gen NamespaceID -> Int -> Gen [BULK]
list _ns 0 = pure []
list ns n = do
    headSize <- chooseInt (1, n)
    let restSize = n - headSize
    (:) <$> resize (headSize - 1) (bulk ns) <*> list ns restSize
