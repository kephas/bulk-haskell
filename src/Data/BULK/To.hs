{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Data.BULK.To where

import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Polysemy (Embed, Member, Members, Sem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Output (Output)
import Polysemy.Reader (Reader, ask, runReader)

import Data.BULK.Core (encodeInt, importNS, version)
import Data.BULK.Core qualified as Core
import Data.BULK.Encode (encodeNat, encodeSeq)
import Data.BULK.Eval ()
import Data.BULK.Types (BULK (..), Context (..), Name (..), Namespace (..), NamespaceID (..), Ref (..), Warning)
import Data.BULK.Utils (bulkToList, liftMaybe, writeFileLBS)

class ToBULK a where
    toBULK :: a -> Encoder BULK

toBULKWith :: (Members [Error String, Output Warning] r, ToBULK a) => Context -> a -> Sem r BULK
toBULKWith ctx value =
    runEncoder ctx $ toBULK value

{- | Encode a value as a binary BULK stream
   | Create necessary imports and use them to encode BULK expressions
-}
encode :: (Members [Error String, Output Warning, Reader Context] r, ToBULK a) => a -> Sem r ByteString
encode value = do
    ctx <- ask
    runEncoder ctx $ toBULK value >>= encodeStream

encodeFile :: (Members [Error String, Output Warning, Reader Context, Embed IO] r, ToBULK a) => FilePath -> a -> Sem r ()
encodeFile path value =
    encode value >>= writeFileLBS path

{- | Encode a BULK expression as a binary BULK stream
   | Create necessary imports and use them to encode BULK expressions
-}
encodeStream :: (Member (Error String) r) => BULK -> Sem r ByteString
encodeStream = encodeStreamSeq . bulkToList

{- | Encode a sequence of BULK expressions as a binary BULK stream
   | Create necessary imports and use them to encode BULK expressions
-}
encodeStreamSeq :: (Member (Error String) r) => [BULK] -> Sem r ByteString
encodeStreamSeq exprs = do
    let nsMarkers = M.fromList $ (`zip` [0x14 ..]) $ S.toAscList $ collectNSS $ Form exprs
        imports = map (unassociateNSS nsMarkers) $ mapMaybe (uncurry importNS) $ M.toAscList nsMarkers
        unassociated = map (unassociateNSS nsMarkers) exprs
    encodeSeq $ [version 1 0] <> imports <> unassociated

collectNSS :: BULK -> S.Set NamespaceID
collectNSS Nil = S.empty
collectNSS (Array _bs) = S.empty
collectNSS (Form exprs) = S.unions $ map collectNSS exprs
collectNSS (Reference (Ref ns _name)) = collectNS ns

collectNS :: NamespaceID -> S.Set NamespaceID
collectNS CoreNS = S.empty
collectNS (UnassociatedNS _) = S.empty
collectNS ns@(MatchEq _) = S.singleton ns
collectNS ns@(MatchNamePrefix _index _digest) = S.singleton ns
collectNS ns1@(MatchQualifiedNamePrefix (Ref ns2 _name) _digest) = S.fromList [ns1, ns2]

unassociateNSS :: M.Map NamespaceID Int -> BULK -> BULK
unassociateNSS _imports Nil = Nil
unassociateNSS _imports array@(Array _) = array
unassociateNSS imports (Form exprs) = Form $ map (unassociateNSS imports) exprs
unassociateNSS imports ref@(Reference (Ref ns name))
    | Just marker <- M.lookup ns imports = Reference $ Ref (UnassociatedNS marker) name
    | otherwise = ref

namedRef :: (Members [Error String, Reader Context] r) => Text -> Text -> Sem r BULK
namedRef nsMnemonic nameMnemonic = do
    ctx <- ask
    ns <- liftMaybe [i|no namespace #{nsMnemonic}|] $ S.lookupMin $ S.filter ((== nsMnemonic) . (.mnemonic)) $ ctx.namespaces
    name <- liftMaybe [i|no name #{ns}:#{nameMnemonic}|] $ find ((== (Just nameMnemonic)) . (.mnemonic)) ns.names
    pure $ Reference $ Ref ns.matchID name

instance ToBULK BULK where
    toBULK = pure

instance ToBULK Bool where
    toBULK True = pure Core.True
    toBULK False = pure Core.False

instance ToBULK Int where
    toBULK num
        | num >= 0 = pure $ encodeNat num
        | otherwise = pure $ encodeInt num

instance (ToBULK a) => ToBULK [a] where
    toBULK xs = Form <$> traverse toBULK xs

instance ToBULK Text where
    toBULK = toBULK . B.fromStrict . encodeUtf8

instance ToBULK ByteString where
    toBULK = pure . Array

instance ToBULK StrictByteString where
    toBULK = toBULK . B.fromStrict

type Encoder a = Sem '[Reader Context, Error String] a

runEncoder :: (Member (Error String) r) => Context -> Encoder a -> Sem r a
runEncoder ctx = either throw pure . run . runError . runReader ctx
