{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Data.BULK.Utils where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Map.Strict qualified as M
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Polysemy (InterpreterFor, Member, Sem)
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail)
import Polysemy.State (State, evalState, get)

import Data.BULK.Types (Namespace (..), NamespaceID, Result (..))

insertIfMissing :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insertIfMissing = M.insertWith (\_new old -> old)

bareNS :: NamespaceID -> Namespace
bareNS nsId = Namespace{matchID = nsId, mnemonic = "", names = []}

runErrorToFail :: (Member Fail r) => InterpreterFor (Error String) r
runErrorToFail = runError >=> either fail pure

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

leftIn :: String -> Either String a -> Either String a
leftIn place = first \err -> [i|#{place}: #{err}|]

failLeftIn :: (HasCallStack) => FilePath -> Either String a -> IO a
failLeftIn file = failLeft . leftIn file

failLeft :: (HasCallStack) => Either String a -> IO a
failLeft = either fail pure

errIn :: String -> Result a -> Result a
errIn place = errMap \err -> [i|#{place}: #{err}|]

errMap :: (String -> String) -> Result a -> Result a
errMap f (Result res) = Result $ first f res

liftFailIn :: (HasCallStack) => FilePath -> Result a -> IO a
liftFailIn file = liftFail . errIn file

liftFail :: (HasCallStack) => Result a -> IO a
liftFail = either fail pure . (.unResult)
