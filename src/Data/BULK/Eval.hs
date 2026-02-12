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

module Data.BULK.Eval (eval, mkContext, evalExpr, execContext, parseText) where

import Control.Category ((>>>))
import Control.Lens (Lens', Prism', at, lens, over, preview, view, (&), (^.), (^?), _1, _Just)
import Control.Monad (join)
import Control.Monad.Extra (whenJust)
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
import Data.BULK.Lens (associatedNamespaces, coreName, definitions, knownNamespaces, knownPackages, lastingNamespaces, _Digest, _Expression, _LazyFunction)
import Data.BULK.Types (BULK (..), CheckDigest, Context (..), Name (..), NameDefinition (..), Namespace (..), NamespaceID (..), Package (..), Scope (..), TypeMismatch (..), Value (..))
import Data.BULK.Types qualified as Fun (LazyFunction (..))
import Data.BULK.Types qualified as ND (Namespace (..))
import Data.BULK.Utils (evalLocalState, runLocalState, (<$$$>))

eval :: Context -> BULK -> Either String BULK
eval (Context scope) bulk =
    leftNothing "nothing yielded" $ runEval scope $ initNSS >> evalExpr bulk
  where
    runEval scope' = run . runError . evalState scope'
    initNSS = traverse_ applyNS scope._knownNamespaces

mkContext :: [Namespace] -> Context
mkContext nss = fromRight emptyContext $ execContext $ traverse_ applyNS $ coreNS : nss

emptyContext :: Context
emptyContext = Context emptyScope

execContext :: Sem '[State Scope, Error String] a -> Either String Context
execContext = (Context <$>) . run . runError . execState emptyScope

emptyScope :: Scope
emptyScope = Scope{_associatedNamespaces = M.empty, _definitions = M.empty, _knownNamespaces = [], _lastingNamespaces = [], _knownPackages = []}

coreNS :: Namespace
coreNS =
    Namespace
        { matchID = CoreNS
        , mnemonic = "bulk"
        , names =
            [ coreName Core.Version "version" $ LazyFunction Fun.Version
            , coreName Core.Import "import" $ LazyFunction Fun.Import
            , coreName Core.Define "define" $ LazyFunction Fun.Define
            , coreName Core.Mnemonic "mnemonic" $ LazyFunction Fun.Mnemonic
            ]
        }

applyNS :: (Member (State Scope) r) => Namespace -> Sem r ()
applyNS ns = do
    knowNS ns
    modify (over definitions $ flip (foldl' insert) ns.names)
  where
    insert defs (NameDefinition{marker, value}) =
        M.insert (Name ns.matchID marker) value defs

knowNS :: (Member (State Scope) r) => Namespace -> Sem r ()
knowNS =
    modify . over knownNamespaces . insertNS
  where
    insertNS ns = M.insert ns.matchID ns

evalExpr :: (Members [State Scope, Error String] r) => BULK -> Sem r (Maybe BULK)
evalExpr Nil = yield Nil
evalExpr arr@(Array _) = yield arr
evalExpr (Reference{name}) = retrieveExpression name
evalExpr (Form content) = evalForm content

yield :: (Applicative f) => a -> f (Maybe a)
yield = pure . Just

noYield :: (Applicative f) => f (Maybe a)
noYield = pure Nothing

retrieveExpression :: (Member (State Scope) r) => Name -> Sem r (Maybe BULK)
retrieveExpression unqualifiedName = do
    (name, value) <- retrieveDefinition _Expression unqualifiedName
    mnemonic <- retrieveMnemonic name
    yield $ fromMaybe (Reference{..}) value

retrieveMnemonic :: (Member (State Scope) r) => Name -> Sem r (Maybe Text)
retrieveMnemonic (Name ns num) =
    gets (^? knownNamespaces . at ns . _Just . nsNameMap . at num . _Just . _1)

retrieveDefinition :: (Member (State Scope) r) => Prism' Value a -> Name -> Sem r (Name, Maybe a)
retrieveDefinition prism name = do
    qualifiedName <- qualifyName name
    (qualifiedName,) <$> gets (view definitions >>> M.lookup qualifiedName >>> (preview prism <$>) >>> join)

qualifyName :: (Member (State Scope) r) => Name -> Sem r Name
qualifyName (Name unassoc@(UnassociatedNS marker) name) = do
    ns <- fromMaybe unassoc <$> gets (fmap (.matchID) . getMarkerNS marker)
    pure $ Name ns name
qualifyName name@(Name _ _) = pure name

getMarkerNS :: Int -> Scope -> Maybe Namespace
getMarkerNS marker scope = do
    match <- scope ^. associatedNamespaces . at marker
    scope ^. knownNamespaces . at match

evalForm :: (Members [State Scope, Error String] r) => [BULK] -> Sem r (Maybe BULK)
evalForm (ref@Reference{} : args) = do
    (qualifiedName, mfun) <- getLazyFunction <$$$> retrieveDefinition _LazyFunction ref.name
    mnemonic <- retrieveMnemonic qualifiedName
    let evalQualifiedForm = Just . Form . (Reference{name = qualifiedName, mnemonic} :) <$> evalAll args
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
    modify $ over knownNamespaces $ flip M.union $ M.restrictKeys scope._knownNamespaces scope._lastingNamespaces
    modify $ over lastingNamespaces $ S.union scope._lastingNamespaces
    modify $ over knownPackages $ S.union scope._knownPackages
    pure $ catMaybes result

getLazyFunction :: Fun.LazyFunction -> forall r. (Members [State Scope, Error TypeMismatch, Error String] r) => [BULK] -> Sem r (Maybe BULK)
getLazyFunction Fun.Version = coreVersion
getLazyFunction Fun.Import = coreImport
getLazyFunction Fun.Define = coreDefine
getLazyFunction Fun.Mnemonic = coreMnemonic

coreVersion, coreImport, coreDefine, coreMnemonic :: (Members [State Scope, Error TypeMismatch, Error String] r) => [BULK] -> Sem r (Maybe BULK)
coreVersion [Nat @Int _, Nat @Int _] =
    noYield
coreVersion _ = throw TypeMismatch
coreImport [Nat base, Nat count, expr] =
    importPackage base count expr
coreImport [Nat marker, Form [Core.Namespace, expr]] = do
    importNS marker expr
coreImport [Nat base, Form [Core.Package, expr, Nat count]] =
    importPackage base count expr
coreImport _ = throw TypeMismatch
coreDefine [Reference{name}, expr] =
    defineReference name expr
coreDefine [Form [Core.Namespace, identifier@(Form [Reference{name = digestName}, Array nsDigest]), Nat marker], Core.False, Array defBytes] = do
    nested <- parseNested defBytes
    case (digestName, nested) of
        (Name (UnassociatedNS marker') name, Form (Nil : mnemonic : _doc : defs))
            | marker' == marker ->
                verifyBootstrappedNS identifier marker name nsDigest mnemonic defBytes defs
        _ ->
            throw TypeMismatch
coreDefine [Form [Core.Namespace, Form [Reference{name = digestName}, Array nsDigest], Nat marker], Array defBytes] = do
    defineNamespace digestName nsDigest marker defBytes
coreDefine [Form [Core.Package, Form [Reference{name = digestName}, Array pkgDigest]], Array defBytes] = do
    nested <- parseNested defBytes
    case nested of
        Form (Nil : nss) ->
            verifyQualifiedPackage digestName pkgDigest nss
        _ ->
            throw TypeMismatch
coreDefine _ = throw TypeMismatch
coreMnemonic [Form [Core.Namespace, Nat marker], mnemonicB] =
    setNSMnemonic marker mnemonicB
coreMnemonic [Reference{name}, mnemonicB] =
    setRefMnemonic name mnemonicB
coreMnemonic _ = throw TypeMismatch

importNS :: (Members [State Scope, Error String] r) => Int -> BULK -> Sem r (Maybe BULK)
importNS marker expr =
    notYielding $ findNS expr >>= associateNS marker

associateNS :: (Members [State Scope, Error String] r) => Int -> Maybe NamespaceID -> Sem r ()
associateNS marker foundNS =
    modify (over associatedNamespaces (M.alter (const foundNS) marker))

findNS :: (Members [State Scope, Error String] r) => BULK -> Sem r (Maybe NamespaceID)
findNS expr = do
    qualifiedExpr <- evalExpr1 expr
    gets (fmap (.matchID) . find (matchOn qualifiedExpr) . view knownNamespaces)

evalExpr1 :: (Members [State Scope, Error String] r) => BULK -> Sem r BULK
evalExpr1 expr =
    fromMaybe expr <$> evalExpr expr

addPackage :: (Members [State Scope, Error String] r) => NamespaceID -> [BULK] -> Sem r (Maybe BULK)
addPackage match nss = do
    foundNSS <- traverse findNS nss
    notYielding $ modify $ over knownPackages $ S.insert Package{matchID = match, nsIDs = foundNSS}

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
    notYielding $ traverse (uncurry associateNS) $ zip (take count [base ..]) foundNSS

defineReference :: (Members [State Scope, Error String] r) => Name -> BULK -> Sem r (Maybe BULK)
defineReference name expr = do
    qualifiedName <- qualifyName name
    notYielding $ modify (over definitions (M.insert qualifiedName $ Expression expr))

defineNamespace :: (Members [State Scope, Error String] r) => Name -> ByteString -> Int -> ByteString -> Sem r (Maybe BULK)
defineNamespace digestName nsDigest marker toDigest = do
    (qualifiedName, digest) <- retrieveDigest digestName
    nested <- parseNested toDigest
    case (runCheckDigest digest nsDigest toDigest, nested) of
        (Right (), Form (Nil : defs)) -> do
            definition <- evalLocalState do
                let newNS = Namespace{matchID = MatchQualifiedNamePrefix qualifiedName nsDigest, mnemonic = "", names = []}
                knowNS newNS
                associateNS marker $ Just newNS.matchID
                traverse_ evalExpr defs
                applyDefinitionsToNS marker
                gets $ getMarkerNS marker
            whenJust definition \ns -> do
                applyNS ns
                modify $ over lastingNamespaces $ S.insert ns.matchID
                associateNS marker $ Just ns.matchID
            noYield
        (Right (), bulk) ->
            throw [i|syntax error in namespace definition: #{debug bulk}|]
        (Left err, _bulk) -> do
            throw [i|verification failed for namespace (#{err})|]

applyDefinitionsToNS :: (Members [State Scope, Error String] r) => Int -> Sem r ()
applyDefinitionsToNS marker = do
    scope <- get
    let mayNS = getMarkerNS marker scope
    case mayNS of
        Just ns -> do
            let nsDefs = M.toList $ M.filterWithKey isNSName $ scope ^. definitions
                isNSName (Name ns' _name) _value = ns' == ns.matchID
                nsWithDefs = ns & over nsNameMap (\nm -> foldl' applyDefinition nm nsDefs)
            knowNS nsWithDefs
            associateNS marker $ Just ns.matchID
        Nothing -> do
            nss <- gets $ view associatedNamespaces
            throw
                [i|no namesapce associated with: #{marker}
#{debug nss}|]

type NameMap = M.Map Word8 (Text, Value)

nsNameMap :: Lens' Namespace NameMap
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

verifyBootstrappedNS :: (Members [State Scope, Error String] r) => BULK -> Int -> Word8 -> ByteString -> BULK -> ByteString -> [BULK] -> Sem r (Maybe BULK)
verifyBootstrappedNS identifier marker name nsDigest mnemonic toDigest defs = do
    mnemonicS <- parseText mnemonic
    foundNS <- gets $ find (matchOn identifier) . view knownNamespaces
    case foundNS of
        Just ns -> do
            void $ modify (over associatedNamespaces (M.insert marker ns.matchID))
            verifyQualifiedNS (Name ns.matchID name) nsDigest marker mnemonic toDigest defs
        Nothing ->
            throw [i|unable to bootstrap namespace: #{mnemonicS}|]

verifyQualifiedNS :: (Members [State Scope, Error String] r) => Name -> ByteString -> Int -> BULK -> ByteString -> [BULK] -> Sem r (Maybe BULK)
verifyQualifiedNS digestName nsDigest marker mnemonic toDigest defs = do
    (_qualifiedName, digest) <- retrieveDigest digestName
    mnemonicS <- parseText mnemonic
    case runCheckDigest digest nsDigest toDigest of
        Right () -> do
            mayNS <- evalLocalState do
                traverse_ evalExpr defs
                gets $ getMarkerNS marker
            case mayNS of
                Just ns -> do
                    applyNS ns
                    knowNS ns
                    modify $ over lastingNamespaces $ S.insert ns.matchID
                    notYielding $ associateNS marker $ Just ns.matchID
                Nothing ->
                    throw [i|definitions lost for namespace: #{mnemonicS}|]
        Left err -> do
            throw [i|verification failed for namespace: #{mnemonicS} (#{err})|]

retrieveDigest :: (Members [State Scope, Error String] r) => Name -> Sem r (Name, CheckDigest)
retrieveDigest name =
    retrieveDefinition _Digest name >>= maybe (throw [i|unknown digest: #{name}|]) pure . sequenceA

setNSMnemonic :: (Members [State Scope, Error String] r) => Int -> BULK -> Sem r (Maybe BULK)
setNSMnemonic marker mnemonicB = notYielding $ do
    mnemonic <- parseText mnemonicB
    foundNS <- gets $ getMarkerNS marker
    whenJust foundNS \oldNS -> do
        let newNS = oldNS{ND.mnemonic}
        associateNS marker $ Just oldNS.matchID
        knowNS newNS

setRefMnemonic :: (Members [State Scope, Error String] r) => Name -> BULK -> Sem r (Maybe BULK)
setRefMnemonic (Name (UnassociatedNS marker) name) mnemonicB = notYielding do
    mnemonic <- parseText mnemonicB
    foundNS <- gets (getMarkerNS marker)
    whenJust foundNS \ns -> do
        modify $ over (knownNamespaces . at ns.matchID . _Just . nsNameMap) $ M.alter (changeMnemonic mnemonic) name
setRefMnemonic _name _mnemonicB =
    throw "not implemented"

changeMnemonic :: Text -> Maybe (Text, Value) -> Maybe (Text, Value)
changeMnemonic mnemonic Nothing = Just (mnemonic, SelfEval)
changeMnemonic mnemonic (Just (_old, value)) = Just (mnemonic, value)

matchOn :: (HasField "matchID" r NamespaceID) => BULK -> r -> Bool
matchOn expr thing = runMatchID thing.matchID expr

runMatchID :: NamespaceID -> BULK -> Bool
runMatchID (MatchEq bulk1) bulk2 =
    bulk1 == bulk2
runMatchID (MatchNamePrefix name referenceDigest) (Form [Reference{name = (Name (UnassociatedNS _) name')}, Array targetDigest])
    | name == name' =
        toStrict targetDigest `isPrefixOf` toStrict referenceDigest
runMatchID (MatchNamePrefix _name _digest) _bulk =
    False
runMatchID (MatchQualifiedNamePrefix (Name ns name) referenceDigest) (Form [Reference{name = (Name ns' name')}, Array targetDigest])
    | ns == ns' && name == name' =
        toStrict targetDigest `isPrefixOf` toStrict referenceDigest
runMatchID (MatchQualifiedNamePrefix _name _digest) _bulk =
    False
runMatchID CoreNS _ =
    False
runMatchID TempNS _ =
    False
runMatchID (UnassociatedNS _) _ =
    False

notYielding :: (Functor f) => f a -> f (Maybe BULK)
notYielding = (Nothing <$)

leftNothing :: e -> Either e (Maybe a) -> Either e a
leftNothing err = (>>= maybe (Left err) Right)

parseNested :: (Member (Error String) r) => ByteString -> Sem r BULK
parseNested = either throw pure . parseStreamV1

parseText :: (Member (Error String) r) => BULK -> Sem r Text
parseText (Array bs) = either (throw . show) pure $ decodeUtf8' $ toStrict bs
parseText bulk = throw [i|not an array: #{bulk}|]
