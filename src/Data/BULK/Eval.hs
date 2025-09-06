module Data.BULK.Eval where

import Data.BULK.Decode (BULK (Form, Reference))
import Data.Map.Strict qualified as M
import Debug.Trace

eval :: [BULK] -> [BULK]
eval = evalScope M.empty

type Scope = M.Map BULK BULK

evalScope :: Scope -> [BULK] -> [BULK]
evalScope _scope [] = []
evalScope scope (Form [Reference 16 9, ref, expr] : rest) =
    evalScope (M.insert ref expr scope) rest
evalScope scope (expr : rest)
    | Just value <- M.lookup expr scope = value : evalScope scope rest
    | otherwise = expr : evalScope scope rest
