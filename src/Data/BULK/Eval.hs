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
{-# LANGUAGE TypeOperators #-}

module Data.BULK.Eval (eval, mkContext, evalExpr, execContext, toText) where

import Control.Category ((>>>))
import Control.Lens (Lens', Prism', at, lens, over, preview, set, view, (&), (^.), _Just)
import Control.Monad (join)
import Control.Monad.Extra (whenJust)
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
import Polysemy.State (State, evalState, execState, get, gets, modify)

import Data.BULK.Core qualified as Core
import Data.BULK.Debug (debug)
import Data.BULK.Decode (parseStreamV1)
import Data.BULK.Encode (encode, pattern Nat)
import Data.BULK.Hash (isPrefixOf, runCheckDigest)
import Data.BULK.Lens (associatedNamespaces, coreName, definingNamespace, definitions, knownNamespaces, knownPackages, lastingNamespaces, namespaceDefinition, nextName, _Digest, _Expression, _LazyFunction)
import Data.BULK.Types (BULK (..), CheckDigest, Context (..), IncompleteNamespace (..), MatchID (..), Name (..), NameDefinition (..), Namespace (..), NamespaceDefinition (..), Package (..), Scope (..), TypeMismatch (..), Value (..))
import Data.BULK.Types qualified as Fun (LazyFunction (..))
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
            [ coreName Core.Version "version" $ LazyFunction Fun.Version
            , coreName Core.Import "import" $ LazyFunction Fun.Import
            , coreName Core.Define "define" $ LazyFunction Fun.Define
            , coreName Core.Mnemonic "mnemonic" $ LazyFunction Fun.Mnemonic
            , coreName Core.MnemonicDef "mnemonic/def" $ LazyFunction Fun.DefineMnemonic
            ]
        }

applyNS :: (Member (State Scope) r) => NamespaceDefinition -> Sem r ()
applyNS ns = do
    modify (over knownNamespaces $ S.insert ns)
    modify (over definitions $ flip (foldl' insert) ns.names)
  where
    insert defs (NameDefinition{marker, value}) =
        M.insert (Name actualNS marker) value defs
    actualNS = if ns == coreNS then CoreNamespace else AssociatedNamespace ns

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
    (scope, result) <- runLocalState $ traverse evalExpr exprs
    void $ modify $ over knownNamespaces $ S.union scope._lastingNamespaces
    void $ modify $ over lastingNamespaces $ S.union scope._lastingNamespaces
    void $ modify $ over knownPackages $ S.union scope._knownPackages
    pure $ catMaybes result

getLazyFunction :: Fun.LazyFunction -> forall r. (Members [State Scope, Error TypeMismatch, Error String] r) => [BULK] -> Sem r (Maybe BULK)
getLazyFunction Fun.Version = coreVersion
getLazyFunction Fun.Import = coreImport
getLazyFunction Fun.Define = coreDefine
getLazyFunction Fun.Mnemonic = coreMnemonic
getLazyFunction Fun.DefineMnemonic = coreDefineMnemonic

coreVersion, coreImport, coreDefine, coreMnemonic, coreDefineMnemonic :: (Members [State Scope, Error TypeMismatch, Error String] r) => [BULK] -> Sem r (Maybe BULK)
coreVersion [Nat @Int _, Nat @Int _] =
    pure Nothing
coreVersion _ = throw TypeMismatch
coreImport [Nat base, Nat count, expr] =
    importPackage base count expr
coreImport [Nat marker, Form [Core.Namespace, expr]] = do
    importNS marker expr
coreImport [Nat base, Form [Core.Package, expr, Nat count]] =
    importPackage base count expr
coreImport _ = throw TypeMismatch
coreDefine [Reference name, expr] =
    defineReference name expr
coreDefine [Form [Core.Namespace, identifier@(Form [Reference digestName, Array nsDigest]), Nat marker], Core.False, Array defBytes] = do
    nested <- parseNested defBytes
    case (digestName, nested) of
        (Name (UnassociatedNamespace marker') name, Form (Nil : mnemonic : _doc : defs))
            | marker' == marker ->
                verifyBootstrappedNS identifier marker name nsDigest mnemonic defBytes defs
        (_, Form (Nil : mnemonic : _doc : defs)) ->
            verifyQualifiedNS digestName nsDigest marker mnemonic defBytes defs
        _ ->
            throw TypeMismatch
coreDefine [Form [Core.Namespace, Form [Reference digestName, Array nsDigest], Nat marker], Array defBytes] = do
    defineNamespace digestName nsDigest marker defBytes
coreDefine [Form [Core.Package, Form [Reference digestName, Array pkgDigest]], Array defBytes] = do
    nested <- parseNested defBytes
    case nested of
        Form (Nil : nss) ->
            verifyQualifiedPackage digestName pkgDigest nss
        _ ->
            throw TypeMismatch
coreDefine _ = throw TypeMismatch
coreMnemonic [Form [Core.Namespace, Nat marker], mnemonicB] =
    setNSMnemonic marker mnemonicB
coreMnemonic [Reference name, mnemonicB] =
    setRefMnemonic name mnemonicB
coreMnemonic _ = throw TypeMismatch
coreDefineMnemonic [Nil, mnemonic, _doc, value] =
    defineImplicit mnemonic $ Just value
coreDefineMnemonic [Nil, mnemonic, _doc] =
    defineImplicit mnemonic Nothing
coreDefineMnemonic _ = throw TypeMismatch

importNS :: (Members [State Scope, Error String] r) => Int -> BULK -> Sem r (Maybe BULK)
importNS marker expr =
    noYield $ findNS expr >>= associateNS marker

associateNS :: (Members [State Scope, Error String] r) => Int -> Maybe NamespaceDefinition -> Sem r ()
associateNS marker foundNS =
    modify (over associatedNamespaces (M.alter (const foundNS) marker))

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

importPackage :: (Members [State Scope, Error String] r) => Int -> Int -> BULK -> Sem r (Maybe BULK)
importPackage base count expr = do
    qualifiedExpr <- evalExpr1 expr
    foundNSS <- maybe [] (.nsIDs) <$> gets (find (matchOn qualifiedExpr) . view knownPackages)
    noYield $ traverse (uncurry associateNS) $ zip (take count [base ..]) foundNSS

defineReference :: (Members [State Scope, Error String] r) => Name -> BULK -> Sem r (Maybe BULK)
defineReference name expr = do
    qualifiedName <- qualifyName name
    noYield $ modify (over definitions (M.insert qualifiedName $ Expression expr))

defineNamespace :: (Members [State Scope, Error String] r) => Name -> ByteString -> Int -> ByteString -> Sem r (Maybe BULK)
defineNamespace digestName nsDigest marker toDigest = do
    (qualifiedName, digest) <- retrieveDigest digestName
    nested <- parseNested toDigest
    case (runCheckDigest digest nsDigest toDigest, nested) of
        (Right (), Form (Nil : defs)) -> do
            definition <- evalLocalState do
                let newNS = NamespaceDefinition{matchID = MatchQualifiedNamePrefix qualifiedName nsDigest, mnemonic = "", names = []}
                associateNS marker $ Just newNS
                traverse_ evalExpr defs
                applyDefinitionsToNS marker
                gets $ view $ associatedNamespaces . at marker
            whenJust definition applyNS
            associateNS marker definition
            pure Nothing
        (Right (), bulk) ->
            throw [i|syntax error in namespace definition: #{debug bulk}|]
        (Left err, _bulk) -> do
            throw [i|verification failed for namespace (#{err})|]

applyDefinitionsToNS :: (Members [State Scope, Error String] r) => Int -> Sem r ()
applyDefinitionsToNS marker = do
    scope <- get
    let mayNS = scope ^. associatedNamespaces . at marker
    case mayNS of
        Just ns -> do
            let nsDefs = M.toList $ M.filterWithKey isNSName $ scope ^. definitions
                isNSName (Name (AssociatedNamespace ns') _name) _value = ns' == ns
                isNSName _name _value = False
                nsWithDefs = ns & over nsNameMap (\nm -> foldl' applyDefinition nm nsDefs)
            associateNS marker $ Just nsWithDefs
        Nothing -> do
            nss <- gets $ view associatedNamespaces
            throw
                [i|no namesapce associated with: #{marker}
#{debug nss}|]

type NameMap = M.Map Word8 (Text, Value)

nsNameMap :: Lens' NamespaceDefinition NameMap
nsNameMap =
    lens getMap setMap
  where
    getMap =
        M.fromList . map fromName . (.names)
    fromName name =
        (name.marker, (name.mnemonic, name.value))
    setMap nsDef nsMap =
        nsDef{names = map toName $ M.toList nsMap}
    toName (marker, (mnemonic, value)) =
        NameDefinition{..}

applyDefinition :: NameMap -> (Name, Value) -> NameMap
applyDefinition nameMap (Name _ns name, value) =
    M.alter (changeDefinition value) name nameMap

changeDefinition :: Value -> Maybe (Text, Value) -> Maybe (Text, Value)
changeDefinition value Nothing = Just ("", value)
changeDefinition value (Just (mnemonic, _old)) = Just (mnemonic, value)

defineImplicit :: (Members [State Scope, Error String] r) => BULK -> Maybe BULK -> Sem r (Maybe BULK)
defineImplicit mnemonicExpr maybeExpr = do
    let mnemonic = fromRight "" $ toText mnemonicExpr
    nsDef <- gets (view definingNamespace)
    case nsDef of
        Just incompleteNS -> do
            value <- case maybeExpr of
                Just expr -> Expression <$> evalExpr1 expr
                Nothing -> pure SelfEval
            let marker = incompleteNS._nextName
                newName = NameDefinition{..}
            noYield $ modify (set definingNamespace $ Just $ over namespaceDefinition (addName newName) $ over nextName (+ 1) incompleteNS)
        Nothing ->
            throw [i|nil marker outside of namespace definition for name: #{mnemonic}|]

verifyBootstrappedNS :: (Members [State Scope, Error String] r) => BULK -> Int -> Word8 -> ByteString -> BULK -> ByteString -> [BULK] -> Sem r (Maybe BULK)
verifyBootstrappedNS identifier marker name nsDigest mnemonic toDigest defs = do
    let mnemonicS = fromRight "" $ toText mnemonic
    foundNS <- gets $ find (matchOn identifier) . view knownNamespaces
    case foundNS of
        Just ns -> do
            void $ modify (over associatedNamespaces (M.insert marker ns))
            verifyQualifiedNS (Name (AssociatedNamespace ns) name) nsDigest marker mnemonic toDigest defs
        Nothing ->
            throw [i|unable to bootstrap namespace: #{mnemonicS}|]

verifyQualifiedNS :: (Members [State Scope, Error String] r) => Name -> ByteString -> Int -> BULK -> ByteString -> [BULK] -> Sem r (Maybe BULK)
verifyQualifiedNS digestName nsDigest marker mnemonic toDigest defs = do
    (qualifiedName, digest) <- retrieveDigest digestName
    let mnemonicS = fromRight "" $ toText mnemonic
    case runCheckDigest digest nsDigest toDigest of
        Right () -> do
            let newNS = Just IncompleteNamespace{_namespaceDefinition = NamespaceDefinition{matchID = MatchQualifiedNamePrefix qualifiedName nsDigest, mnemonic = mnemonicS, names = []}, _nextName = 0}
            mayNS <- evalLocalState do
                void $ modify $ set definingNamespace newNS
                traverse_ evalExpr defs
                gets $ view definingNamespace
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

setNSMnemonic :: (Members [State Scope, Error String] r) => Int -> BULK -> Sem r (Maybe BULK)
setNSMnemonic marker mnemonicB = do
    mnemonic <- either throw pure $ toText mnemonicB
    foundNS <- gets $ view $ associatedNamespaces . at marker
    case foundNS of
        Just oldNS -> do
            let newNS = oldNS{ND.mnemonic}
            associateNS marker $ Just newNS
            noYield $ modify $ over knownNamespaces $ S.insert newNS
        Nothing ->
            pure Nothing

setRefMnemonic :: (Members [State Scope, Error String] r) => Name -> BULK -> Sem r (Maybe BULK)
setRefMnemonic (Name (UnassociatedNamespace marker) name) mnemonicB = do
    mnemonic <- either throw pure $ toText mnemonicB
    modify $ over (associatedNamespaces . at marker . _Just . nsNameMap) $ M.alter (changeMnemonic mnemonic) name
    pure Nothing
setRefMnemonic _name _mnemonicB =
    throw "not implemented"

changeMnemonic :: Text -> Maybe (Text, Value) -> Maybe (Text, Value)
changeMnemonic mnemonic Nothing = Just (mnemonic, SelfEval)
changeMnemonic mnemonic (Just (_old, value)) = Just (mnemonic, value)

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

parseNested :: (Member (Error String) r) => ByteString -> Sem r BULK
parseNested = either throw pure . parseStreamV1

toText :: BULK -> Either String Text
toText (Array bs) = first show $ decodeUtf8' $ toStrict bs
toText bulk = Left [i|not an array: #{bulk}|]

addName :: NameDefinition -> NamespaceDefinition -> NamespaceDefinition
addName name nsDef =
    nsDef{ND.names = name : nsDef.names}

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

runLocalState :: (Member (State s) r) => Sem (State s : r) a -> Sem r (s, a)
runLocalState action = evalLocalState do
    result <- action
    state_ <- get
    pure (state_, result)

evalLocalState :: (Member (State s) r) => Sem (State s : r) a -> Sem r a
evalLocalState action = do
    state_ <- get
    evalState state_ action
