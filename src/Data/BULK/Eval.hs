{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BULK.Eval (eval, evalCtx, mkContext, emptyScope, evalExpr, execContext, parseText) where

import Control.Lens (Prism', at, over, preview, to, view, (^.), (^?), _Just)
import Control.Monad.Extra (whenJust)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import GHC.Records (HasField)
import Polysemy (Member, Members, Sem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.State (State, evalState, execState, gets, modify, put)

import Data.BULK.Core qualified as Core
import Data.BULK.Debug (debug, detraceState)
import Data.BULK.Decode (parseStreamV1)
import Data.BULK.Encode (pattern Nat)
import Data.BULK.Hash (isPrefixOf, runCheckDigest)
import Data.BULK.Lens (associatedNamespaces, coreName, knownNS, knownNamespaces, knownPackages, lastingNamespaces, nameMap, nsName, setNsName, _Digest, _Expression, _LazyFunction)
import Data.BULK.Types (BULK (..), CheckDigest, Context (..), Name (..), Namespace (..), NamespaceID (..), Package (..), Ref (..), Scope (..), TypeMismatch (..), Value (..), Warning (..), withKey)
import Data.BULK.Types qualified as Fun (LazyFunction (..))
import Data.BULK.Types qualified as N (Name (..))
import Data.BULK.Types qualified as NS (Namespace (..))
import Data.BULK.Utils (bareNS, evalLocalState, insertIfMissing, runLocalState, runWarningsAndError, (<$$$>))
import Polysemy.Output (Output, ignoreOutput, output)
import Witch (from)

eval :: Context -> BULK -> Either String BULK
eval ctx =
    runEval . evalCtx ctx
  where
    runEval = run . ignoreOutput . runError . evalState emptyScope

evalCtx :: (Members [State Scope, Error String, Output Warning] r) => Context -> BULK -> Sem r BULK
evalCtx ctx bulk = do
    put $ from ctx
    evalExpr bulk >>= maybe (error "nothing yielded") pure

mkContext :: [Namespace] -> Context
mkContext nss = Context S.empty $ S.fromList $ coreNS : nss

instance Monoid Context where
    mempty = Context S.empty $ S.singleton coreNS

execContext :: Sem '[State Scope, Error String, Output Warning] a -> Either String Context
execContext = (from <$>) . run . runWarningsAndError . execState emptyScope

emptyScope :: Scope
emptyScope = from @Context mempty

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
            , coreName Core.Trace "trace" $ LazyFunction Fun.Trace
            ]
        }

knowNS :: (Member (State Scope) r) => Namespace -> Sem r ()
knowNS = insertNS M.insert

ensureNS :: (Member (State Scope) r) => Namespace -> Sem r ()
ensureNS = insertNS insertIfMissing

type NamespaceMap = M.Map NamespaceID Namespace
type InsertNS = NamespaceID -> Namespace -> NamespaceMap -> NamespaceMap

insertNS :: (Member (State Scope) r) => InsertNS -> Namespace -> Sem r ()
insertNS insert =
    modify . over knownNamespaces . uncurry insert . withKey

evalExpr :: (Members [State Scope, Output Warning, Error String] r) => BULK -> Sem r (Maybe BULK)
evalExpr Nil = yield Nil
evalExpr arr@(Array _) = yield arr
evalExpr (Reference ref) = retrieveExpression ref
evalExpr (Form content) = evalForm content

yield :: (Applicative f) => a -> f (Maybe a)
yield = pure . Just

noYield :: (Applicative f) => f (Maybe a)
noYield = pure Nothing

retrieveExpression :: (Member (State Scope) r) => Ref -> Sem r (Maybe BULK)
retrieveExpression unqualifiedRef = do
    (ref, value) <- retrieveDefinition _Expression unqualifiedRef
    yield $ fromMaybe (Reference ref) value

retrieveDefinition :: (Member (State Scope) r) => Prism' Value a -> Ref -> Sem r (Ref, Maybe a)
retrieveDefinition prism ref = do
    qualRef <- qualifyRef ref
    (qualRef,) <$> gets (preview $ knownNS qualRef.nsID . nsName qualRef.name.marker . to (.value) . prism)

qualifyRef :: (Member (State Scope) r) => Ref -> Sem r Ref
qualifyRef (Ref ns name) = do
    qualNS <- qualifyNS ns
    qualName <- fromMaybe name <$> gets (^? knownNS qualNS . nsName name.marker)
    pure $ Ref qualNS qualName

qualifyNS :: (Member (State Scope) r) => NamespaceID -> Sem r NamespaceID
qualifyNS unassoc@(UnassociatedNS marker) =
    fromMaybe unassoc <$> gets (fmap (.matchID) . getMarkerNS marker)
qualifyNS ns = pure ns

getMarkerNS :: Int -> Scope -> Maybe Namespace
getMarkerNS marker scope = do
    match <- scope ^. associatedNamespaces . at marker
    scope ^. knownNamespaces . at match

evalForm :: (Members [State Scope, Output Warning, Error String] r) => [BULK] -> Sem r (Maybe BULK)
evalForm (Reference ref : args) = do
    (qualRef, mfun) <- getLazyFunction <$$$> retrieveDefinition _LazyFunction ref
    let evalQualifiedForm = Just . Form . (Reference qualRef :) <$> evalAll args
    case mfun of
        Nothing ->
            evalQualifiedForm
        Just fun -> do
            result <- runError (fun args)
            case result of
                Left TypeMismatch -> evalQualifiedForm
                Right bulk -> pure bulk
evalForm content =
    Just . Form <$> evalAll content

evalAll :: (Members [State Scope, Output Warning, Error String] r) => [BULK] -> Sem r [BULK]
evalAll exprs = do
    (scope, result) <- runLocalState $ traverse evalExpr exprs
    modify $ over knownNamespaces $ flip M.union $ M.restrictKeys scope._knownNamespaces scope._lastingNamespaces
    modify $ over lastingNamespaces $ S.union scope._lastingNamespaces
    modify $ over knownPackages $ S.union scope._knownPackages
    pure $ catMaybes result

getLazyFunction :: Fun.LazyFunction -> forall r. (Members [State Scope, Error TypeMismatch, Output Warning, Error String] r) => [BULK] -> Sem r (Maybe BULK)
getLazyFunction Fun.Version = coreVersion
getLazyFunction Fun.Import = coreImport
getLazyFunction Fun.Define = coreDefine
getLazyFunction Fun.Mnemonic = coreMnemonic
getLazyFunction Fun.Trace = coreTrace

coreVersion, coreImport, coreDefine, coreMnemonic, coreTrace :: (Members [State Scope, Error TypeMismatch, Output Warning, Error String] r) => [BULK] -> Sem r (Maybe BULK)
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
coreDefine [Reference ref, expr] =
    defineReference ref expr
coreDefine [Form [Core.Namespace, Form [Reference digestRef, Array nsDigest], Nat marker], Array defBytes] = do
    defineNamespace digestRef nsDigest marker defBytes
coreDefine [Form [Core.Package, Form [Reference digestRef, Array pkgDigest]], Array defBytes] = do
    definePackage digestRef pkgDigest defBytes
coreDefine _ = throw TypeMismatch
coreMnemonic [Form [Core.Namespace, Nat marker], mnemonicB] =
    setNSMnemonic marker mnemonicB
coreMnemonic [Reference ref, mnemonicB] =
    setRefMnemonic ref mnemonicB
coreMnemonic _ = throw TypeMismatch
coreTrace [] =
    notYielding detraceState
coreTrace _ = throw TypeMismatch

importNS :: (Members [State Scope, Output Warning, Error String] r) => Int -> BULK -> Sem r (Maybe BULK)
importNS marker expr =
    notYielding $ findNS expr >>= associateNS marker

associateNS :: (Members [State Scope, Error String] r) => Int -> Maybe NamespaceID -> Sem r ()
associateNS marker foundNS =
    modify (over associatedNamespaces (M.alter (const foundNS) marker))

findNS :: (Members [State Scope, Output Warning, Error String] r) => BULK -> Sem r (Maybe NamespaceID)
findNS expr = do
    qualifiedExpr <- evalExpr1 expr
    gets (fmap (.matchID) . find (matchOn qualifiedExpr) . view knownNamespaces)

evalExpr1 :: (Members [State Scope, Output Warning, Error String] r) => BULK -> Sem r BULK
evalExpr1 expr =
    fromMaybe expr <$> evalExpr expr

importPackage :: (Members [State Scope, Output Warning, Error String] r) => Int -> Int -> BULK -> Sem r (Maybe BULK)
importPackage base count expr = notYielding do
    qualifiedExpr <- evalExpr1 expr
    mayPkg <- gets (find (matchOn qualifiedExpr) . view knownPackages)
    case (qualifiedExpr, mayPkg) of
        (_, Just pkg) ->
            traverse_ (uncurry associateNS) $ zip (take count [base ..]) pkg.nsIDs
        (Form [Reference _, Array digest], Nothing) ->
            output $ Warning [i|unknown package: #{debug digest}|]
        _ ->
            output $ Warning "unknown weird package"

defineReference :: (Members [State Scope, Error String] r) => Ref -> BULK -> Sem r (Maybe BULK)
defineReference ref expr = notYielding $ do
    qualRef <- qualifyRef ref
    ensureNS $ bareNS qualRef.nsID
    modify $ over (knownNS qualRef.nsID . setNsName qualRef.name.marker) $ changeValue $ qualRef.name{value = Expression expr}

changeValue :: Name -> Maybe Name -> Maybe Name
changeValue name Nothing = Just name
changeValue name (Just old) = Just name{N.mnemonic = old.mnemonic}

defineNamespace :: (Members [State Scope, Output Warning, Error String] r) => Ref -> ByteString -> Int -> ByteString -> Sem r (Maybe BULK)
defineNamespace digestRef nsDigest marker toDigest = do
    let defineNS = case digestRef of
            Ref (UnassociatedNS marker') _ | marker == marker' -> defineBootstrappedNamespace
            _ -> defineQualifiedNamespace
    defineNS digestRef nsDigest marker toDigest

defineBootstrappedNamespace :: (Members [State Scope, Output Warning, Error String] r) => Ref -> ByteString -> Int -> ByteString -> Sem r (Maybe BULK)
defineBootstrappedNamespace digestRef nsDigest marker toDigest = do
    foundNS <- gets $ find (matchOn $ Form [Reference digestRef, Array nsDigest]) . view knownNamespaces
    case foundNS of
        Just ns -> do
            associateNS marker $ Just ns.matchID
            defineQualifiedNamespace (Ref ns.matchID digestRef.name) nsDigest marker toDigest
        Nothing -> do
            ns <- evalNS marker toDigest $ Namespace TempNS "<unnamed>" []
            let mnemonic = ns.mnemonic
            throw [i|unable to bootstrap namespace: #{mnemonic}|]

defineQualifiedNamespace :: (Members [State Scope, Output Warning, Error String] r) => Ref -> ByteString -> Int -> ByteString -> Sem r (Maybe BULK)
defineQualifiedNamespace digestRef nsDigest marker toDigest = do
    (qualifiedRef, digest) <- retrieveDigest digestRef
    nested <- parseNested toDigest
    case (runCheckDigest digest nsDigest toDigest, nested) of
        (Right (), Form (Nil : defs)) -> notYielding do
            definition <- evalLocalState do
                let newNS = Namespace{matchID = MatchQualifiedNamePrefix qualifiedRef nsDigest, mnemonic = "", names = []}
                knowNS newNS
                associateNS marker $ Just newNS.matchID
                traverse_ evalExpr defs
                gets $ getMarkerNS marker
            whenJust definition \ns -> do
                knowNS ns
                modify $ over lastingNamespaces $ S.insert ns.matchID
                associateNS marker $ Just ns.matchID
        (Right (), bulk) ->
            throw [i|syntax error in namespace definition: #{debug bulk}|]
        (Left err, _bulk) -> do
            throw [i|verification failed for namespace (#{err})|]

evalNS :: (Members [State Scope, Output Warning, Error String] r) => Int -> ByteString -> Namespace -> Sem r Namespace
evalNS marker defBytes newNS = do
    nested <- parseNested defBytes
    case nested of
        Form (Nil : defs) -> evalLocalState do
            knowNS newNS
            associateNS marker $ Just newNS.matchID
            traverse_ evalExpr defs
            gets (getMarkerNS marker) >>= maybe (throw "lost namespace definition") pure
        bulk ->
            throw [i|syntax error in namespace definition: #{debug bulk}|]

retrieveDigest :: (Members [State Scope, Error String] r) => Ref -> Sem r (Ref, CheckDigest)
retrieveDigest name =
    retrieveDefinition _Digest name >>= maybe (throw [i|unknown digest: #{name}|]) pure . sequenceA

definePackage :: (Members [State Scope, Output Warning, Error String] r) => Ref -> ByteString -> ByteString -> Sem r (Maybe BULK)
definePackage digestRef pkgDigest defBytes = notYielding do
    (qualifiedRef, digest) <- retrieveDigest digestRef
    nested <- parseNested defBytes
    case (runCheckDigest digest pkgDigest defBytes, nested) of
        (Right (), Form (Nil : nss)) -> notYielding do
            foundNSS <- traverse findNS nss
            let pkgID =
                    if Just qualifiedRef.nsID `elem` foundNSS
                        then MatchNamePrefix digestRef.name.marker pkgDigest
                        else MatchQualifiedNamePrefix qualifiedRef pkgDigest
            modify $ over knownPackages $ S.insert Package{matchID = pkgID, nsIDs = foundNSS}
        (Right (), bulk) ->
            throw [i|syntax error in package definition: #{debug bulk}|]
        (Left err, _bulk) -> do
            throw [i|verification failed for package (#{err})|]

setNSMnemonic :: (Members [State Scope, Error String] r) => Int -> BULK -> Sem r (Maybe BULK)
setNSMnemonic marker mnemonicB = notYielding $ do
    mnemonic <- parseText mnemonicB
    foundNS <- gets $ getMarkerNS marker
    whenJust foundNS \oldNS -> do
        let newNS = oldNS{NS.mnemonic}
        associateNS marker $ Just oldNS.matchID
        knowNS newNS

setRefMnemonic :: (Members [State Scope, Error String] r) => Ref -> BULK -> Sem r (Maybe BULK)
setRefMnemonic (Ref (UnassociatedNS marker) name) mnemonicB = notYielding do
    mnemonic <- parseText mnemonicB
    foundNS <- gets (getMarkerNS marker)
    whenJust foundNS \ns -> do
        modify $ over (knownNamespaces . at ns.matchID . _Just . nameMap) $ M.alter (changeMnemonic $ Name name.marker (Just mnemonic) SelfEval) name.marker
setRefMnemonic _name _mnemonicB =
    throw "not implemented"

changeMnemonic :: Name -> Maybe Name -> Maybe Name
changeMnemonic nd Nothing = Just nd
changeMnemonic nd (Just old) = Just nd{value = old.value}

matchOn :: (HasField "matchID" r NamespaceID) => BULK -> r -> Bool
matchOn expr thing = runMatchID thing.matchID expr

runMatchID :: NamespaceID -> BULK -> Bool
runMatchID (MatchEq bulk1) bulk2 =
    bulk1 == bulk2
runMatchID (MatchNamePrefix name fullDigest) (Form [Reference (Ref (UnassociatedNS _) name'), Array targetDigest])
    | name == name'.marker =
        toStrict targetDigest `isPrefixOf` toStrict fullDigest
runMatchID nsID1@(MatchNamePrefix nameMarker1 fullDigest) (Form [Reference (Ref nsID2 name2), Array targetDigest])
    | nsID1 == nsID2 && nameMarker1 == name2.marker =
        toStrict targetDigest `isPrefixOf` toStrict fullDigest
runMatchID (MatchNamePrefix _name _digest) _bulk =
    False
runMatchID (MatchQualifiedNamePrefix (Ref ns name) fullDigest) (Form [Reference (Ref ns' name'), Array targetDigest])
    | ns == ns' && name == name' =
        toStrict targetDigest `isPrefixOf` toStrict fullDigest
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

parseNested :: (Member (Error String) r) => ByteString -> Sem r BULK
parseNested = either throw pure . parseStreamV1

parseText :: (Member (Error String) r) => BULK -> Sem r Text
parseText (Array bs) = either (throw . show) pure $ decodeUtf8' $ toStrict bs
parseText bulk = throw [i|not an array: #{bulk}|]
