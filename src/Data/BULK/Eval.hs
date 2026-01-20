{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Eval where

import Control.Lens (makeLenses, over, set, view, (<|~))
import Data.Bifunctor (first)
import Data.ByteString (toStrict)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import GHC.Records (HasField)
import Polysemy (Member, Members, Sem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.State (State, evalState, execState, get, gets, modify)

import Data.BULK.Core (pattern Core)
import Data.BULK.Encode (encode, pattern Nat)
import Data.BULK.Types (BULK (..), FullNamespaceDefinition (..), NameDefinition (..), Namespace (..), NamespaceDefinition (..), Package (..))
import Data.BULK.Types qualified as ND (NamespaceDefinition (..))
import Data.BULK.Unsafe (defineNamespace)

data Scope = Scope
    { _associatedNamespaces :: M.Map Int FullNamespaceDefinition
    , _definitions :: M.Map BULK BULK
    , _knownNamespaces :: [FullNamespaceDefinition]
    , _knownPackages :: [Package]
    , _definingNamespace :: Maybe IncompleteNamespace
    }

data IncompleteNamespace = IncompleteNamespace
    { _namespaceDefinition :: NamespaceDefinition
    , _nextName :: Word8
    , _pendingDefinitions :: [(Word8, BULK)]
    }

makeLenses ''Scope
makeLenses ''IncompleteNamespace

emptyScope :: [FullNamespaceDefinition] -> Scope
emptyScope nss = Scope{_associatedNamespaces = M.empty, _definitions = M.empty, _knownNamespaces = nss, _knownPackages = [], _definingNamespace = Nothing}

eval :: [FullNamespaceDefinition] -> BULK -> Either String BULK
eval nss bulk = leftNothing "nothing yielded" $ runEval (emptyScope nss) $ evalExpr bulk

evalExpr :: (Members [State Scope, Error String] r) => BULK -> Sem r (Maybe BULK)
evalExpr (Form [Core 0x00, Nat @Int _, Nat @Int _]) =
    pure Nothing
evalExpr (Form [Core 0x03, Nat marker, expr]) =
    associateNS marker expr
evalExpr (Form (Core 0x04 : identifier@(Array _) : nss)) = do
    noYield $ modify (knownPackages <|~ Package{matchID = (== identifier), nsIDs = nss})
evalExpr (Form [Core 0x05, Nat base, Nat count, expr]) = do
    foundNSS <- maybe [] (.nsIDs) <$> gets (find (matchOn expr) . view knownPackages)
    noYield $ traverse (uncurry associateNS) $ zip (take count [base ..]) foundNSS
evalExpr (Form [Core 0x06, ref, expr]) =
    Nothing <$ modify (over definitions (M.insert ref expr))
evalExpr (Form [Core 0x07, Nil, mnemonic, _doc, value]) = do
    let mnemonicS = fromRight "" $ toText mnemonic
    nsDef <- gets (view definingNamespace)
    case nsDef of
        Just incompleteNS -> do
            let newName = SelfEval{marker = incompleteNS._nextName, mnemonic = mnemonicS}
                newDef = (incompleteNS._nextName, value)
            noYield $ modify (set definingNamespace $ Just $ over namespaceDefinition (addName newName) $ over nextName (+ 1) $ over pendingDefinitions (newDef :) incompleteNS)
        Nothing ->
            throw [i|nil marker outside of namespace definition for name: #{mnemonicS}|]
evalExpr (Form (Core 0x09 : Form [digestRef, Array nsDigest] : toDigest@(Nat marker : Nil : mnemonic : _doc : defs))) = do
    qualifiedDigestRef <- throwNothing [i|failed to qualify reference: {digestRef}|] $ evalExpr digestRef
    digestName <- getName qualifiedDigestRef
    case digestName of
        Just (DigestName{checkDigest}) -> do
            let reencoded = encode toDigest
                mnemonicS = fromRight "" $ toText mnemonic
            case checkDigest nsDigest reencoded of
                Right () -> do
                    scope <- get
                    let newNS = Just IncompleteNamespace{_namespaceDefinition = NamespaceDefinition{matchID = const False, mnemonic = mnemonicS, names = []}, _nextName = 0, _pendingDefinitions = []}
                        localScope = scope{_definingNamespace = newNS}
                    mayNS <- (view definingNamespace <$>) . execState localScope $ traverse evalExpr defs
                    case mayNS of
                        Just incompleteNS -> do
                            let definedNS = defineNamespace incompleteNS._namespaceDefinition
                                completeDefinitions = map (first $ Reference (AssociatedNamespace marker definedNS)) incompleteNS._pendingDefinitions
                            traverse_ (\(ref, value) -> modify $ over definitions $ M.insert ref value) completeDefinitions
                            void $ modify (over knownNamespaces (definedNS :))
                            noYield $ modify (over associatedNamespaces (M.insert marker definedNS))
                        Nothing ->
                            throw [i|definitions lost for namespace: #{mnemonicS}|]
                Left err -> do
                    throw [i|verification failed for namespace: #{mnemonicS} (#{err})|]
        _ -> pure Nothing
evalExpr (Form content) = do
    scope <- get
    (Just . Form . catMaybes <$>) . evalState scope $ traverse evalExpr content
evalExpr (Reference unassoc@(UnassociatedNamespace marker) name) = do
    ns <- fromMaybe unassoc <$> gets (fmap (AssociatedNamespace marker) . M.lookup marker . view associatedNamespaces)
    retrieveDefinition $ Reference ns name
evalExpr expr =
    retrieveDefinition expr
runEval :: Scope -> Sem '[State Scope, Error String] a -> Either String a
runEval scope = run . runError . evalState scope

retrieveDefinition :: (Member (State Scope) r) => BULK -> Sem r (Maybe BULK)
retrieveDefinition expr =
    Just <$> gets (M.findWithDefault expr expr . view definitions)

associateNS :: (Member (State Scope) r) => Int -> BULK -> Sem r (Maybe BULK)
associateNS marker expr = do
    foundNS <- gets (find (matchOn expr) . view knownNamespaces)
    noYield $ modify (over associatedNamespaces (M.alter (const foundNS) marker))

getName :: (Member (State Scope) r) => BULK -> Sem r (Maybe NameDefinition)
getName (Reference (AssociatedNamespace _ ns) name) = do
    pure $ find ((== name) . (.marker)) ns.names
getName _bulk = pure Nothing

matchOn :: (HasField "matchID" r (BULK -> Bool)) => BULK -> r -> Bool
matchOn expr thing = thing.matchID expr

noYield :: (Functor f) => f a -> f (Maybe BULK)
noYield = (Nothing <$)

leftNothing :: e -> Either e (Maybe a) -> Either e a
leftNothing err = (>>= maybe (Left err) Right)

throwNothing :: (Member (Error e) r) => e -> Sem r (Maybe a) -> Sem r a
throwNothing err = (>>= maybe (throw err) pure)

toText :: BULK -> Either String Text
toText (Array bs) = first show $ decodeUtf8' $ toStrict bs
toText bulk = Left [i|not an array: #{bulk}|]

addName :: NameDefinition -> NamespaceDefinition -> NamespaceDefinition
addName name nsDef =
    nsDef{ND.names = name : nsDef.names}
