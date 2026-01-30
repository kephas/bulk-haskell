{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.Debug (Debug (..), module Debug.Trace) where

import Data.BULK.Eval.Types
import Data.BULK.Types
import Data.ByteString.Lazy (ByteString)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Debug.Trace
import Text.Hex qualified as H
import Witch (from)

class Debug a where
    debug :: a -> String
    default debug :: (Show a) => a -> String
    debug = show

instance Debug BULK where
    debug Nil = "nil"
    debug (Form content) = "( " ++ unwords (map debug content) ++ " )"
    debug (Array bs) = [i|[#{debug bs}]|]
    debug (Reference name) = debug name

instance Debug ByteString where
    debug = from . H.lazilyEncodeHex

instance Debug MatchID where
    debug MatchNone = "!"
    debug (MatchEq bulk) = [i|== #{debug bulk}|]
    debug (MatchNamePrefix num bs) = [i|#{num} <<< #{debug bs}|]
    debug (MatchQualifiedNamePrefix _name bs) = [i|<<< #{debug bs}|]

instance Debug Name where
    debug (Name ns num) = [i|#{debug ns}:#{num}|]

instance Debug Namespace where
    debug CoreNamespace = "{core}"
    debug (UnassociatedNamespace num) = [i|{#{num}}|]
    debug (AssociatedNamespace ns) = [i|{NS #{debug ns}}|]

instance Debug NamespaceDefinition where
    debug NamespaceDefinition{..} = [i|#{mnemonic}(#{length names}, #{debug matchID})|]

instance Debug Package where
    debug Package{..} = [i|{pkg (#{debug matchID}) #{map debug nsIDs}|]

instance Debug Context where
    debug (Context scope) = debug scope

instance (Debug a) => Debug (Maybe a) where
    debug Nothing = "<>"
    debug (Just x) = [i|<#{debug x}>|]

instance (Show k, Debug v) => Debug (M.Map k v) where
    debug = show . map (debug <$>) . M.toList

instance (Debug v) => Debug (S.Set v) where
    debug = show . map debug . S.toList

instance Debug Scope where
    debug Scope{..} = [i|ANSS: #{debug _associatedNamespaces}, KNSS: #{debug _knownNamespaces}, LNSS: #{debug _lastingNamespaces}, KPKG: #{debug _knownPackages}|]
