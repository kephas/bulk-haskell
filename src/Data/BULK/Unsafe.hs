{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

module Data.BULK.Unsafe where

import Data.BULK.Types (FullNamespaceDefinition (..), NamespaceDefinition (..))
import Data.Unique (newUnique)
import System.IO.Unsafe (unsafePerformIO)

defineNamespace :: NamespaceDefinition -> FullNamespaceDefinition
defineNamespace (NamespaceDefinition{..}) =
    FullNamespaceDefinition{..}
  where
    unique = unsafePerformIO newUnique
{-# NOINLINE defineNamespace #-}
