{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Data.BULK.To where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.List (find)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Polysemy (Member, Members, Sem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Output (Output)
import Polysemy.Reader (Reader, ask, runReader)

import Data.BULK.Core (encodeInt)
import Data.BULK.Core qualified as Core
import Data.BULK.Encode (encodeNat)
import Data.BULK.Types (BULK (..), Context (..), Name (..), Namespace (..), Ref (..), Warning)
import Data.BULK.Utils (liftMaybe)

class ToBULK a where
    encodeBULK :: a -> Encoder BULK

toBULK :: (Members [Error String, Output Warning, Reader Context] r, ToBULK a) => a -> Sem r BULK
toBULK value = do
    ctx <- ask
    runEncoder ctx $ encodeBULK value

toBULKWith :: (Members [Error String, Output Warning] r, ToBULK a) => Context -> a -> Sem r BULK
toBULKWith ctx value =
    runReader ctx $ toBULK value

askName :: (Members [Error String, Reader Context] r) => Text -> Text -> Sem r BULK
askName nsMnemonic nameMnemonic = do
    ctx <- ask
    ns <- liftMaybe [i|no namespace #{nsMnemonic}|] $ S.lookupMin $ S.filter ((== nsMnemonic) . (.mnemonic)) $ ctx.namespaces
    name <- liftMaybe [i|no name #{ns}:#{nameMnemonic}|] $ find ((== (Just nameMnemonic)) . (.mnemonic)) ns.names
    pure $ Reference $ Ref ns.matchID name

instance ToBULK Bool where
    encodeBULK True = pure Core.True
    encodeBULK False = pure Core.False

instance ToBULK Int where
    encodeBULK num
        | num >= 0 = pure $ encodeNat num
        | otherwise = pure $ encodeInt num

instance (ToBULK a) => ToBULK [a] where
    encodeBULK xs = Form <$> traverse encodeBULK xs

instance ToBULK Text where
    encodeBULK = encodeBULK . B.fromStrict . encodeUtf8

instance ToBULK ByteString where
    encodeBULK = pure . Array

type Encoder a = Sem '[Reader Context, Error String] a

runEncoder :: (Member (Error String) r) => Context -> Encoder a -> Sem r a
runEncoder ctx = either throw pure . run . runError . runReader ctx
