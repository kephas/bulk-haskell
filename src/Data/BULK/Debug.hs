{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.Debug (
    Debug (..),
    debugState,
    module Debug.Trace,
)
where

import Data.BULK.Types
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Debug.Trace
import Polysemy (Member, Sem)
import Polysemy.State (State, get)
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
    debug (MatchNamePrefix num bs) = [i|#{num} (<<<) #{take 8 $ debug bs}|]
    debug (MatchQualifiedNamePrefix _name bs) = [i|<<< #{take 8 $ debug bs}|]

instance Debug Name where
    debug (Name ns num) = [i|#{shortNS ns}:#{num}|]

instance Debug Namespace where
    debug CoreNamespace = "{core}"
    debug (UnassociatedNamespace num) = [i|{#{num}}|]
    debug (AssociatedNamespace ns) = [i|#{debug ns}}|]

shortNS :: Namespace -> String
shortNS (AssociatedNamespace (NamespaceDefinition{mnemonic})) = [i|{#{mnemonic}}|]
shortNS ns = debug ns

instance Debug NamespaceDefinition where
    debug NamespaceDefinition{..} = [i|#{mnemonic}(#{length names}, #{debug matchID}, #{map debug names})|]

instance Debug NameDefinition where
    debug NameDefinition{marker, mnemonic, value} = [i|#{marker}:#{mnemonic}#{debug value}|]

instance Debug Package where
    debug Package{..} = [i|{pkg (#{debug matchID}) #{debug nsIDs}|]

instance Debug Context where
    debug (Context scope) = debug scope

instance (Debug a) => Debug (Maybe a) where
    debug Nothing = "<>"
    debug (Just x) = [i|<#{debug x}>|]

instance (Debug e, Debug a) => Debug (Either e a) where
    debug (Left err) = [i|<<L #{debug err}>>|]
    debug (Right res) = [i|<<R #{debug res}>>|]

instance (Debug a) => Debug [a] where
    debug as = "[" ++ intercalate ", " (map debug as) ++ "]"

instance (Debug a, Debug b) => Debug (a, b) where
    debug (a, b) = [i|(#{debug a}, #{debug b})|]

instance (Debug k, Debug v) => Debug (M.Map k v) where
    debug = show . map (bimap debug debug) . M.toList

instance Debug Int

instance {-# OVERLAPPING #-} Debug [Char] where
    debug = id

instance Debug T.Text where
    debug = T.unpack

instance (Debug v) => Debug (S.Set v) where
    debug = show . map debug . S.toList

instance Debug Scope where
    debug Scope{..} =
        [i|ANSS: #{debug _associatedNamespaces}, KNSS: #{debug _knownNamespaces}, LNSS: #{debug _lastingNamespaces}, KPKG: #{debug _knownPackages}, DefNS: #{debug _definingNamespace}
Defs: #{debug _definitions}|]

instance Debug IncompleteNamespace where
    debug IncompleteNamespace{..} = [i|{@#{_nextName} + #{debug _namespaceDefinition}|]

instance Debug Value where
    debug SelfEval = "=="
    debug (Expression bulk) = [i|=#{debug bulk}|]
    debug (Digest digest) = [i|=~#{digest}|]
    debug (LazyFunction f) = [i|=#{f}()|]

debugState :: (Member (State s) r, Debug s) => Sem r ()
debugState = get >>= traceM . debug
