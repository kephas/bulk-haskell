{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Eval where

import Control.Lens (makeLenses, over, view, (<|~))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Polysemy (Member, Sem, run)
import Polysemy.State (State, evalState, get, gets, modify)

import Data.BULK.Core (pattern Core)
import Data.BULK.Encode (pattern Nat)
import Data.BULK.Types (BULK (..), FullNamespaceDefinition (..), Namespace (..), Package (..))
import Data.List (find)
import GHC.Records (HasField)

data Scope = Scope
    { _associatedNamespaces :: M.Map Int FullNamespaceDefinition
    , _definitions :: M.Map BULK BULK
    , _knownNamespaces :: [FullNamespaceDefinition]
    , _knownPackages :: [Package]
    }

makeLenses ''Scope

emptyScope :: [FullNamespaceDefinition] -> Scope
emptyScope nss = Scope{_associatedNamespaces = M.empty, _definitions = M.empty, _knownNamespaces = nss, _knownPackages = []}

eval :: [FullNamespaceDefinition] -> BULK -> BULK
eval nss bulk = fromMaybe bulk $ run $ evalState (emptyScope nss) $ evalExpr bulk

evalExpr :: (Member (State Scope) r) => BULK -> Sem r (Maybe BULK)
evalExpr (Form [Core 0x03, Nat marker, expr]) =
    associateNS marker expr
evalExpr (Form (Core 0x04 : identifier@(Array _) : nss)) = do
    noYield $ modify (knownPackages <|~ Package{matchID = (== identifier), nsIDs = nss})
evalExpr (Form [Core 0x05, Nat base, Nat count, expr]) = do
    foundNSS <- maybe [] (.nsIDs) <$> gets (find (matchOn expr) . view knownPackages)
    noYield $ traverse (uncurry associateNS) $ zip (take count [base ..]) foundNSS
evalExpr (Form [Core 0x06, ref, expr]) =
    Nothing <$ modify (over definitions (M.insert ref expr))
evalExpr (Form [Core 0x00, Nat @Int _, Nat @Int _]) =
    pure Nothing
evalExpr (Form content) = do
    scope <- get
    let evaledContent = catMaybes $ run $ evalState scope $ traverse @[] evalExpr content
    pure $ Just $ Form evaledContent
evalExpr (Reference unassoc@(UnassociatedNamespace marker) name) = do
    ns <- fromMaybe unassoc <$> gets (fmap (AssociatedNamespace marker) . M.lookup marker . view associatedNamespaces)
    retrieveDefinition $ Reference ns name
evalExpr expr =
    retrieveDefinition expr

retrieveDefinition :: (Member (State Scope) r) => BULK -> Sem r (Maybe BULK)
retrieveDefinition expr =
    Just <$> gets (M.findWithDefault expr expr . view definitions)

associateNS :: (Member (State Scope) r) => Int -> BULK -> Sem r (Maybe BULK)
associateNS marker expr = do
    foundNS <- gets (find (matchOn expr) . view knownNamespaces)
    noYield $ modify (over associatedNamespaces (M.alter (const foundNS) marker))

matchOn :: (HasField "matchID" r (BULK -> Bool)) => BULK -> r -> Bool
matchOn expr thing = thing.matchID expr

noYield :: (Functor f) => f a -> f (Maybe BULK)
noYield = (Nothing <$)
