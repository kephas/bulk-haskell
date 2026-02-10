{-# LANGUAGE PatternSynonyms #-}

module Data.BULK (
    module Reexport,
) where

import Data.BULK.Core as Reexport (define, encodeInt, toIntegral, version)
import Data.BULK.Decode as Reexport (VersionConstraint (..), getExpression, getStream, parseLazy, parseStream, parseStreamV1, readFile, readFileV1, toNat)
import Data.BULK.Encode as Reexport (encode, encodeNat, pattern Nat)
import Data.BULK.Eval as Reexport (eval)
import Data.BULK.Lens as Reexport (_Array, _BulkExpr, _Form, _Int, _Nat, _Nil, _Reference)
import Data.BULK.TextNotation as Reexport (parseNotation, parseNotationFile, parseNotationFileBin, readTextFile)
import Data.BULK.ToFrom as Reexport (FromBULK (..), ToBULK (..), decode, decodeFile, decodeNotation, decodeNotationFile, fromBULK, loadNotationFiles, nextBULK, nsName, string, withForm, withSequence, withStream, (<*:>), (<:>))
import Data.BULK.Types as Reexport (BULK (..), CheckDigest (..), Context, MatchID (..), Name (..), NameDefinition (..), Namespace (..), NamespaceDefinition (..), Value (..))
