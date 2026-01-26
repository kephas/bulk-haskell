{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

module Data.BULK.Eval where

import Control.Category ((>>>))
import Control.Lens (Prism', over, preview, set, view, (<|~))
import Control.Monad (join)
import Data.Bifunctor (bimap, first)
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
import GHC.Records (HasField)
import Polysemy (Member, Members, Sem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.State (State, evalState, execState, get, gets, modify)

import Data.BULK.Encode (encode, pattern Nat)
import Data.BULK.Eval.Types (IncompleteNamespace (..), Scope (..), Value (..), associatedNamespaces, definingNamespace, definitions, knownNamespaces, knownPackages, namespaceDefinition, nextName, pendingDefinitions, _Digest, _Expression)
import Data.BULK.Hash (isPrefixOf, runCheckDigest)
import Data.BULK.Types (BULK (..), MatchID (..), Name (..), NameDefinition (..), Namespace (..), NamespaceDefinition (..), Package (..), pattern Core)
import Data.BULK.Types qualified as ND (NamespaceDefinition (..))

emptyScope :: Scope
emptyScope = Scope{_associatedNamespaces = M.empty, _definitions = M.empty, _knownNamespaces = [], _knownPackages = [], _definingNamespace = Nothing}

eval :: [NamespaceDefinition] -> BULK -> Either String BULK
eval nss bulk = leftNothing "nothing yielded" $ runEval emptyScope $ traverse_ applyNS nss >> evalExpr bulk

evalExpr :: (Members [State Scope, Error String] r) => BULK -> Sem r (Maybe BULK)
evalExpr Nil = yield Nil
evalExpr arr@(Array _) = yield arr
evalExpr (Reference name) =
    retrieveExpression name
evalExpr (Form [Core 0x00, Data.BULK.Encode.Nat @Int _, Data.BULK.Encode.Nat @Int _]) =
    pure Nothing
evalExpr (Form [Core 0x03, Data.BULK.Encode.Nat marker, expr]) =
    associateNS marker expr
evalExpr (Form (Core 0x04 : identifier@(Array _) : nss)) = do
    noYield $ modify (knownPackages <|~ Package{matchID = MatchEq identifier, nsIDs = nss})
evalExpr (Form [Core 0x05, Data.BULK.Encode.Nat base, Data.BULK.Encode.Nat count, expr]) = do
    foundNSS <- maybe [] (.nsIDs) <$> gets (find (matchOn expr) . view knownPackages)
    noYield $ traverse (uncurry associateNS) $ zip (take count [base ..]) foundNSS
evalExpr (Form [Core 0x06, Reference name, expr]) =
    noYield $ modify (over definitions (M.insert name $ Expression expr))
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
evalExpr (Form (Core 0x09 : identifier@(Form [Reference (Name (UnassociatedNamespace marker) name), Array nsDigest]) : toDigest@(Data.BULK.Encode.Nat marker' : Nil : mnemonic : _doc : _defs)))
    | marker == marker' = do
        let mnemonicS = fromRight "" $ toText mnemonic
        foundNS <- gets $ find (matchOn identifier) . view knownNamespaces
        case foundNS of
            Just ns -> do
                void $ modify (over associatedNamespaces (M.insert marker ns))
                evalExpr $ Form (Core 0x09 : Form [Reference (Name (AssociatedNamespace ns) name), Array nsDigest] : toDigest)
            Nothing ->
                throw [i|unable to bootstrap namespace: #{mnemonicS}|]
evalExpr (Form (Core 0x09 : Form [Reference digestName, Array nsDigest] : toDigest@(Data.BULK.Encode.Nat marker : Nil : mnemonic : _doc : defs))) = do
    digest <- retrieveDefinition _Digest digestName
    case digest of
        Just checkDigest -> do
            let reencoded = Data.BULK.Encode.encode toDigest
                mnemonicS = fromRight "" $ toText mnemonic
            case runCheckDigest checkDigest nsDigest reencoded of
                Right () -> do
                    scope <- get
                    let newNS = Just IncompleteNamespace{_namespaceDefinition = NamespaceDefinition{matchID = MatchNone, mnemonic = mnemonicS, names = []}, _nextName = 0, _pendingDefinitions = []}
                        localScope = scope{_definingNamespace = newNS}
                    mayNS <- (view definingNamespace <$>) . execState localScope $ traverse evalExpr defs
                    case mayNS of
                        Just incompleteNS -> do
                            let definedNS = incompleteNS._namespaceDefinition
                                completeDefinitions = map (bimap (Name (AssociatedNamespace definedNS)) Expression) incompleteNS._pendingDefinitions
                            traverse_ (\(name, value) -> modify $ over definitions $ M.insert name value) completeDefinitions
                            void $ modify (over knownNamespaces (definedNS :))
                            noYield $ modify (over associatedNamespaces (M.insert marker definedNS))
                        Nothing ->
                            throw [i|definitions lost for namespace: #{mnemonicS}|]
                Left err -> do
                    throw [i|verification failed for namespace: #{mnemonicS} (#{err})|]
        _ -> throw "unknown digest"
evalExpr (Form content) = do
    scope <- get
    (Just . Form . catMaybes <$>) . evalState scope $ traverse evalExpr content

runEval :: Scope -> Sem '[State Scope, Error String] a -> Either String a
runEval scope = run . runError . evalState scope

qualifyName :: (Member (State Scope) r) => Name -> Sem r Name
qualifyName name@(Name CoreNamespace _) = pure name
qualifyName name@(Name (AssociatedNamespace _) _) = pure name
qualifyName (Name unassoc@(UnassociatedNamespace marker) name) = do
    ns <- fromMaybe unassoc <$> gets (fmap AssociatedNamespace . M.lookup marker . view associatedNamespaces)
    pure $ Name ns name

retrieveDefinition :: (Member (State Scope) r) => Prism' Value a -> Name -> Sem r (Maybe a)
retrieveDefinition prism name = do
    qualifiedName <- qualifyName name
    gets (view definitions >>> M.lookup qualifiedName >>> (preview prism <$>) >>> join)

retrieveExpression :: (Member (State Scope) r) => Name -> Sem r (Maybe BULK)
retrieveExpression name = do
    qualifiedName <- qualifyName name
    Just . fromMaybe (Reference qualifiedName) <$> retrieveDefinition _Expression name

applyNS :: (Member (State Scope) r) => NamespaceDefinition -> Sem r ()
applyNS ns = do
    modify (over knownNamespaces (ns :))
    traverse_ (\name -> modify (over definitions $ maybeInsert name)) ns.names
  where
    maybeInsert :: NameDefinition -> M.Map Name Value -> M.Map Name Value
    maybeInsert (DigestName{marker, checkDigest}) =
        M.insert (Name (AssociatedNamespace ns) marker) $ Digest checkDigest
    maybeInsert (SelfEval{}) = id

associateNS :: (Member (State Scope) r) => Int -> BULK -> Sem r (Maybe BULK)
associateNS marker expr = do
    foundNS <- gets (find (matchOn expr) . view knownNamespaces)
    noYield $ modify (over associatedNamespaces (M.alter (const foundNS) marker))

getName :: BULK -> Maybe NameDefinition
getName (Reference (Name (AssociatedNamespace ns) name)) = do
    find ((== name) . (.marker)) ns.names
getName _bulk = Nothing

matchOn :: (HasField "matchID" r MatchID) => BULK -> r -> Bool
matchOn expr thing = runMatchID thing.matchID expr

runMatchID :: MatchID -> BULK -> Bool
runMatchID (MatchEq bulk1) bulk2 =
    bulk1 == bulk2
runMatchID (MatchNamePrefix name referenceDigest) (Form [Reference (Name (UnassociatedNamespace _) name'), Array targetDigest])
    | name == name' = toStrict targetDigest `isPrefixOf` toStrict referenceDigest
runMatchID _ _ = False

yield :: (Applicative f) => a -> f (Maybe a)
yield = pure . Just

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
