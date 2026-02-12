{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.BULK.Utils where

import Control.Monad ((>=>))
import Polysemy (InterpreterFor, Member, Sem)
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail)
import Polysemy.State (State, evalState, get)

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
