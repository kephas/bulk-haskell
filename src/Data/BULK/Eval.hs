{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.BULK.Eval where

import Control.Lens (makeLenses, over, view)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Polysemy (Member, Sem, run)
import Polysemy.State (State, evalState, gets, modify)

import Data.BULK.Core (pattern Core)
import Data.BULK.Encode (pattern Nat)
import Data.BULK.Types (BULK (..))

data Scope a b = Scope {_namespaces :: M.Map a b, _definitions :: M.Map BULK BULK}

makeLenses ''Scope

emptyScope :: Scope a b
emptyScope = Scope{_namespaces = M.empty, _definitions = M.empty}

eval :: BULK -> BULK
eval bulk = fromMaybe bulk $ run $ evalState emptyScope $ evalExpr bulk

evalExpr :: (Member (State (Scope a b)) r) => BULK -> Sem r (Maybe BULK)
evalExpr (Form [Core 0x09, ref, expr]) =
    Nothing <$ modify (over definitions (M.insert ref expr))
evalExpr (Form [Core 0x00, Nat @Int _, Nat @Int _]) =
    pure Nothing
evalExpr (Form content) = do
    Just . Form . catMaybes <$> traverse @[] evalExpr content
evalExpr expr =
    Just <$> gets (M.findWithDefault expr expr . view definitions)
