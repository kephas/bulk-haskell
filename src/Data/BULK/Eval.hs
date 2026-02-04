{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

module Data.BULK.Eval (eval, mkContext, evalExpr, execContext, toText) where

import Control.Category ((>>>))
import Control.Lens (Prism', over, preview, set, view)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Either (fromRight)
import Data.Foldable (foldl', traverse_)
import Data.Functor (void)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import GHC.Records (HasField)
import Polysemy (Member, Members, Sem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.State (State, evalState, execState, get, gets, modify, runState)

import Data.BULK.Encode (encode, pattern Nat)
import Data.BULK.Eval.Types
import Data.BULK.Hash (isPrefixOf, runCheckDigest)
import Data.BULK.Types (BULK (..), CheckDigest, MatchID (..), Name (..), NameDefinition (..), Namespace (..), NamespaceDefinition (..), Package (..), pattern Core)
import Data.BULK.Types qualified as Core (LazyFunction (..))
import Data.BULK.Types qualified as ND (NamespaceDefinition (..))

eval :: Context -> BULK -> Either String BULK
eval (Context scope) bulk =
    leftNothing "nothing yielded" $ runEval scope $ initNSS >> evalExpr bulk
  where
    runEval scope' = run . runError . evalState scope'
    initNSS = traverse_ applyNS scope._knownNamespaces

mkContext :: [NamespaceDefinition] -> Context
mkContext nss = fromRight emptyContext $ execContext $ traverse_ applyNS $ coreNS : nss

emptyContext :: Context
emptyContext = Context emptyScope

execContext :: Sem '[State Scope, Error String] a -> Either String Context
execContext = (Context <$>) . run . runError . execState emptyScope

emptyScope :: Scope
emptyScope = Scope{_associatedNamespaces = M.empty, _definitions = M.empty, _knownNamespaces = [], _lastingNamespaces = [], _knownPackages = [], _definingNamespace = Nothing}

coreNS :: NamespaceDefinition
coreNS =
    NamespaceDefinition
        { matchID = MatchNone
        , mnemonic = "bulk"
        , names =
            [ LazyName 0x00 "version" Core.Version
            , LazyName 0x03 "ns" Core.AssociateNS
            , LazyName 0x04 "package" Core.CreatePackage
            , LazyName 0x05 "import" Core.Import
            , LazyName 0x06 "define" Core.Define
            , LazyName 0x07 "mnemonic/def" Core.DefineMnemonic
            , LazyName 0x09 "verifiable-ns" Core.VerifyNS
            ]
        }

applyNS :: (Member (State Scope) r) => NamespaceDefinition -> Sem r ()
applyNS ns = do
    modify (over knownNamespaces $ S.insert ns)
    modify (over definitions $ flip (foldl' maybeInsert) ns.names)
  where
    maybeInsert :: M.Map Name Value -> NameDefinition -> M.Map Name Value
    maybeInsert defs (DigestName{marker, checkDigest}) =
        M.insert (Name (AssociatedNamespace ns) marker) (Digest checkDigest) defs
    maybeInsert defs (ExpressionName{marker, expression}) =
        M.insert (Name (AssociatedNamespace ns) marker) (Expression expression) defs
    maybeInsert defs (LazyName{marker, lazyFunction}) =
        M.insert (Name CoreNamespace marker) (LazyFunction lazyFunction) defs
    maybeInsert defs (SelfEval{}) = defs

evalExpr :: (Members [State Scope, Error String] r) => BULK -> Sem r (Maybe BULK)
evalExpr Nil = yield Nil
evalExpr arr@(Array _) = yield arr
evalExpr (Reference name) = retrieveExpression name
evalExpr (Form content) = evalForm content

yield :: (Applicative f) => a -> f (Maybe a)
yield = pure . Just

retrieveExpression :: (Member (State Scope) r) => Name -> Sem r (Maybe BULK)
retrieveExpression name = do
    (qualifiedName, value) <- retrieveDefinition _Expression name
    yield $ fromMaybe (Reference qualifiedName) value

retrieveDefinition :: (Member (State Scope) r) => Prism' Value a -> Name -> Sem r (Name, Maybe a)
retrieveDefinition prism name = do
    qualifiedName <- qualifyName name
    (qualifiedName,) <$> gets (view definitions >>> M.lookup qualifiedName >>> (preview prism <$>) >>> join)

qualifyName :: (Member (State Scope) r) => Name -> Sem r Name
qualifyName name@(Name CoreNamespace _) = pure name
qualifyName name@(Name (AssociatedNamespace _) _) = pure name
qualifyName (Name unassoc@(UnassociatedNamespace marker) name) = do
    ns <- fromMaybe unassoc <$> gets (fmap AssociatedNamespace . M.lookup marker . view associatedNamespaces)
    pure $ Name ns name

evalForm :: (Members [State Scope, Error String] r) => [BULK] -> Sem r (Maybe BULK)
evalForm (Reference name : args) = do
    (qualifiedName, mfun) <- getLazyFunction <$$$> retrieveDefinition _LazyFunction name
    let evalQualifiedForm = Just . Form . (Reference qualifiedName :) <$> evalAll args
    case mfun of
        Nothing ->
            evalQualifiedForm
        Just fun -> do
            result <- runError @TypeMismatch (fun args)
            case result of
                Left _ -> evalQualifiedForm
                Right bulk -> pure bulk
evalForm content =
    Just . Form <$> evalAll content

evalAll :: (Members [State Scope, Error String] r) => [BULK] -> Sem r [BULK]
evalAll exprs = do
    scope <- get
    (scope', result) <- runState scope (traverse evalExpr exprs)
    void $ modify $ over knownNamespaces $ S.union scope'._lastingNamespaces
    void $ modify $ over lastingNamespaces $ S.union scope'._lastingNamespaces
    void $ modify $ over knownPackages $ S.union scope'._knownPackages
    pure $ catMaybes result

getLazyFunction :: Core.LazyFunction -> forall r. (Members [State Scope, Error TypeMismatch, Error String] r) => [BULK] -> Sem r (Maybe BULK)
getLazyFunction Core.Version = coreVersion
getLazyFunction Core.AssociateNS = coreAssociateNS
getLazyFunction Core.CreatePackage = corePackage
getLazyFunction Core.Import = coreImport
getLazyFunction Core.Define = coreDefine
getLazyFunction Core.DefineMnemonic = coreDefineMnemonic
getLazyFunction Core.VerifyNS = coreVerifyNS

coreVersion, coreAssociateNS, corePackage, coreImport, coreDefine, coreDefineMnemonic, coreVerifyNS :: (Members [State Scope, Error TypeMismatch, Error String] r) => [BULK] -> Sem r (Maybe BULK)
coreVersion [Nat @Int _, Nat @Int _] =
    pure Nothing
coreVersion _ = throw TypeMismatch
coreAssociateNS [Nat marker, expr] =
    findNS expr >>= associateNS marker
coreAssociateNS _ = throw TypeMismatch
corePackage (identifier@(Array _) : nss) =
    addPackage (MatchEq identifier) nss
corePackage (Form [Reference digestName, Array pkgDigest] : Nil : nss) =
    verifyQualifiedPackage digestName pkgDigest nss
corePackage _ = throw TypeMismatch
coreImport [Nat base, Nat count, expr] = do
    qualifiedExpr <- evalExpr1 expr
    foundNSS <- maybe [] (.nsIDs) <$> gets (find (matchOn qualifiedExpr) . view knownPackages)
    noYield $ traverse (uncurry associateNS) $ zip (take count [base ..]) foundNSS
coreImport _ = throw TypeMismatch
coreDefine [Reference name, expr] =
    noYield $ modify (over definitions (M.insert name $ Expression expr))
coreDefine _ = throw TypeMismatch
coreDefineMnemonic [Nil, mnemonic, _doc, value] =
    defineImplicit mnemonic $ Just value
coreDefineMnemonic [Nil, mnemonic, _doc] =
    defineImplicit mnemonic Nothing
coreDefineMnemonic _ = throw TypeMismatch
coreVerifyNS (identifier@(Form (Reference (Name (UnassociatedNamespace marker) name) : idRest)) : toDigest@(Nat marker' : Nil : mnemonic : _))
    | marker == marker' =
        verifyBootstrappedNS identifier marker name idRest mnemonic toDigest
coreVerifyNS (Form [Reference digestName, Array nsDigest] : toDigest@(Nat marker : Nil : mnemonic : _doc : defs)) =
    verifyQualifiedNS digestName nsDigest marker mnemonic toDigest defs
coreVerifyNS _ = throw TypeMismatch

associateNS :: (Members [State Scope, Error String] r) => Int -> Maybe NamespaceDefinition -> Sem r (Maybe BULK)
associateNS marker foundNS =
    noYield $ modify (over associatedNamespaces (M.alter (const foundNS) marker))

findNS :: (Members [State Scope, Error String] r) => BULK -> Sem r (Maybe NamespaceDefinition)
findNS expr = do
    qualifiedExpr <- evalExpr1 expr
    gets (find (matchOn qualifiedExpr) . view knownNamespaces)

evalExpr1 :: (Members [State Scope, Error String] r) => BULK -> Sem r BULK
evalExpr1 expr =
    fromMaybe expr <$> evalExpr expr

addPackage :: (Members [State Scope, Error String] r) => MatchID -> [BULK] -> Sem r (Maybe BULK)
addPackage match nss = do
    foundNSS <- traverse findNS nss
    noYield $ modify $ over knownPackages $ S.insert Package{matchID = match, nsIDs = foundNSS}

verifyQualifiedPackage :: (Members [State Scope, Error String] r) => Name -> ByteString -> [BULK] -> Sem r (Maybe BULK)
verifyQualifiedPackage digestName pkgDigest nss = do
    (qualifiedName, digest) <- retrieveDigest digestName
    let reencoded = encode $ Nil : nss
    case runCheckDigest digest pkgDigest reencoded of
        Right () -> do
            addPackage (MatchQualifiedNamePrefix qualifiedName pkgDigest) nss
        Left err -> do
            throw [i|verification failed for package (#{err})|]

defineImplicit :: (Members [State Scope, Error String] r) => BULK -> Maybe BULK -> Sem r (Maybe BULK)
defineImplicit mnemonic maybeValue = do
    let mnemonicS = fromRight "" $ toText mnemonic
    nsDef <- gets (view definingNamespace)
    case nsDef of
        Just incompleteNS -> do
            newName <- case maybeValue of
                Just value -> do
                    qualifiedValue <- evalExpr1 value
                    pure $ ExpressionName{marker = incompleteNS._nextName, mnemonic = mnemonicS, expression = qualifiedValue}
                Nothing ->
                    pure $ SelfEval{marker = incompleteNS._nextName, mnemonic = mnemonicS}
            noYield $ modify (set definingNamespace $ Just $ over namespaceDefinition (addName newName) $ over nextName (+ 1) incompleteNS)
        Nothing ->
            throw [i|nil marker outside of namespace definition for name: #{mnemonicS}|]

verifyBootstrappedNS :: (Members [State Scope, Error String] r) => BULK -> Int -> Word8 -> [BULK] -> BULK -> [BULK] -> Sem r (Maybe BULK)
verifyBootstrappedNS identifier marker name idRest mnemonic toDigest = do
    let mnemonicS = fromRight "" $ toText mnemonic
    foundNS <- gets $ find (matchOn identifier) . view knownNamespaces
    case foundNS of
        Just ns -> do
            void $ modify (over associatedNamespaces (M.insert marker ns))
            evalExpr $ Form (Core 0x09 : Form (Reference (Name (AssociatedNamespace ns) name) : idRest) : toDigest)
        Nothing ->
            throw [i|unable to bootstrap namespace: #{mnemonicS}|]

verifyQualifiedNS :: (Members [State Scope, Error String] r) => Name -> ByteString -> Int -> BULK -> [BULK] -> [BULK] -> Sem r (Maybe BULK)
verifyQualifiedNS digestName nsDigest marker mnemonic toDigest defs = do
    (qualifiedName, digest) <- retrieveDigest digestName
    let reencoded = encode toDigest
        mnemonicS = fromRight "" $ toText mnemonic
    case runCheckDigest digest nsDigest reencoded of
        Right () -> do
            scope <- get
            let newNS = Just IncompleteNamespace{_namespaceDefinition = NamespaceDefinition{matchID = MatchQualifiedNamePrefix qualifiedName nsDigest, mnemonic = mnemonicS, names = []}, _nextName = 0}
                localScope = scope{_definingNamespace = newNS}
            mayNS <- (view definingNamespace <$>) . execState localScope $ traverse evalExpr defs
            case mayNS of
                Just incompleteNS -> do
                    let definedNS = incompleteNS._namespaceDefinition
                    applyNS definedNS
                    void $ modify $ over knownNamespaces $ S.insert definedNS
                    void $ modify $ over lastingNamespaces $ S.insert definedNS
                    noYield $ modify (over associatedNamespaces (M.insert marker definedNS))
                Nothing ->
                    throw [i|definitions lost for namespace: #{mnemonicS}|]
        Left err -> do
            throw [i|verification failed for namespace: #{mnemonicS} (#{err})|]

retrieveDigest :: (Members [State Scope, Error String] r) => Name -> Sem r (Name, CheckDigest)
retrieveDigest name =
    retrieveDefinition _Digest name >>= maybe (throw [i|unknown digest: #{name}|]) pure . sequenceA

matchOn :: (HasField "matchID" r MatchID) => BULK -> r -> Bool
matchOn expr thing = runMatchID thing.matchID expr

runMatchID :: MatchID -> BULK -> Bool
runMatchID (MatchEq bulk1) bulk2 =
    bulk1 == bulk2
runMatchID (MatchNamePrefix name referenceDigest) (Form [Reference (Name (UnassociatedNamespace _) name'), Array targetDigest])
    | name == name' = toStrict targetDigest `isPrefixOf` toStrict referenceDigest
runMatchID (MatchQualifiedNamePrefix (Name (AssociatedNamespace ns) name) referenceDigest) (Form [Reference (Name (AssociatedNamespace ns') name'), Array targetDigest])
    | ns == ns' && name == name' = toStrict targetDigest `isPrefixOf` toStrict referenceDigest
runMatchID _ _ = False

noYield :: (Functor f) => f a -> f (Maybe BULK)
noYield = (Nothing <$)

leftNothing :: e -> Either e (Maybe a) -> Either e a
leftNothing err = (>>= maybe (Left err) Right)

toText :: BULK -> Either String Text
toText (Array bs) = first show $ decodeUtf8' $ toStrict bs
toText bulk = Left [i|not an array: #{bulk}|]

addName :: NameDefinition -> NamespaceDefinition -> NamespaceDefinition
addName name nsDef =
    nsDef{ND.names = name : nsDef.names}

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap
