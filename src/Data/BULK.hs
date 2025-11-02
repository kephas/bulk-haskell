{-# LANGUAGE PatternSynonyms #-}

module Data.BULK (
    module Reexport,
) where

import Data.BULK.Core as Reexport (define, encodeInt, toIntegral, version)
import Data.BULK.Decode as Reexport (VersionConstraint (..), getExpression, getStream, parseLazy, readFile, readFileWithVersion, toNat)
import Data.BULK.Encode as Reexport (encode, encodeNat, pattern Nat)
import Data.BULK.Eval as Reexport (eval)
import Data.BULK.Lens as Reexport (_Array, _BulkExpr, _Form, _Int, _Nat, _Nil, _Reference)
import Data.BULK.TextNotation as Reexport (parseTextFile, parseTextFileWith, parseTextNotation)
import Data.BULK.ToFrom as Reexport (FromBULK (..), decode, decodeFile, decodeNotation, decodeNotationFile, fromBULK, nextBULK, nsName, rawName, withForm, withSequence, withStream)
import Data.BULK.Types as Reexport (BULK (..), FullNamespaceDefinition, NameDefinition (..), Namespace (..), NamespaceDefinition (..))
import Data.BULK.Unsafe as Reexport (defineNamespace)
