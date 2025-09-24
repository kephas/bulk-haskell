{-# LANGUAGE PatternSynonyms #-}

module Data.BULK (
    module Data.BULK.Decode,
    module Data.BULK.Encode,
    module Data.BULK.Lens,
    module Data.BULK.TextNotation,
    module Data.BULK.Eval,
    module Data.BULK.Core,
) where

import Data.BULK.Core (define, encodeInt, toIntegral, version)
import Data.BULK.Decode (BULK (..), VersionConstraint (..), getExpression, getStream, parseLazy, readFile, readFileWithVersion, toNat)
import Data.BULK.Encode (encode, encodeNat, pattern Nat)
import Data.BULK.Eval (eval)
import Data.BULK.Lens (_Array, _BulkExpr, _Form, _Int, _Nat, _Nil, _Reference)
import Data.BULK.TextNotation (parseTextFile, parseTextFileWith, parseTextNotation)
import Prelude hiding (readFile)
