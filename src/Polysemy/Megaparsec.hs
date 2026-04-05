{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Polysemy.Megaparsec where

import Control.Applicative (Alternative (..))
import Control.Monad.Fail qualified as Fail
import Data.Bifunctor (first)
import Data.Set qualified as S
import Polysemy (InterpreterFor, Member, Sem, makeSem)
import Polysemy.Error (Error, throw)
import Polysemy.Final (Final, embedFinal, interpretFinal, liftS, runFinal, runS)
import Polysemy.NonDet (NonDet (..))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPL

-- MonadParsec

data MonadParsec e s m a where
    Label :: String -> m a -> MonadParsec e s m a
    Try :: m a -> MonadParsec e s m a

makeSem ''MonadParsec

type MegaparsecSem e s r = (Member (Final (MP.Parsec e s)) r, Ord e, MP.Stream s)

eof :: (MegaparsecSem e s r) => Sem r ()
eof = embedFinal MP.eof

token :: (MegaparsecSem e s r) => (MP.Token s -> Maybe a) -> S.Set (MP.ErrorItem (MP.Token s)) -> Sem r a
token = embed2 MP.token

tokens :: (MegaparsecSem e s r) => (MP.Tokens s -> MP.Tokens s -> Bool) -> MP.Tokens s -> Sem r (MP.Tokens s)
tokens = embed2 MP.tokens

takeWhileP :: (MegaparsecSem e s r) => Maybe String -> (MP.Token s -> Bool) -> Sem r (MP.Tokens s)
takeWhileP = embed2 MP.takeWhileP

takeWhile1P :: (MegaparsecSem e s r) => Maybe String -> (MP.Token s -> Bool) -> Sem r (MP.Tokens s)
takeWhile1P = embed2 MP.takeWhile1P

takeP :: (MegaparsecSem e s r) => Maybe String -> Int -> Sem r (MP.Tokens s)
takeP = embed2 MP.takeP

-- Other functions

space1 :: (MegaparsecSem e s r, MP.Token s ~ Char) => Sem r ()
space1 = embedFinal MPC.space1

decimal :: (Num a, MegaparsecSem e s r, MP.Token s ~ Char) => Sem r a
decimal = embedFinal MPL.decimal

chunk :: (MegaparsecSem e s r) => MP.Tokens s -> Sem r (MP.Tokens s)
chunk = embedFinal . MP.chunk

single :: (MegaparsecSem e s r) => MP.Token s -> Sem r (MP.Token s)
single = embedFinal . MP.single

fail :: (MonadFail m, Member (Final m) r) => String -> Sem r a
fail = embedFinal . Fail.fail

-- Helpers

embed2 :: (Member (Final m) r, Functor m) => (a -> b -> m c) -> a -> b -> Sem r c
embed2 f a b = embedFinal $ f a b

-- Interpreters

runMonadParsec :: (Member (Final (MP.Parsec e s)) r, Ord e, MP.Stream s) => InterpreterFor (MonadParsec e s) r
runMonadParsec = interpretFinal \case
    Label name sem -> do
        parser <- runS sem
        pure $ MP.label name parser
    Try sem -> do
        parser <- runS sem
        pure $ MP.try parser

runNonDetFinal :: (Member (Final m) r, Alternative m) => InterpreterFor NonDet r
runNonDetFinal = interpretFinal \case
    Empty -> liftS empty
    Choose semA semB -> do
        a <- runS semA
        b <- runS semB
        pure $ a <|> b

runMegaparsec :: (Member (Error String) r, Ord e, MP.Stream s) => (MP.ParseErrorBundle s e -> String) -> String -> s -> Sem '[MonadParsec e s, NonDet, Final (MP.Parsec e s)] a -> Sem r a
runMegaparsec mapError name input =
    either throw pure . first mapError . runParser . runFinal . runNonDetFinal . runMonadParsec
  where
    runParser parser = MP.runParser parser name input
