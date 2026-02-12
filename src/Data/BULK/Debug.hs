{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.Debug (
    Debug (..),
    traceD,
    traceIdD,
    traceDM,
    debugState,
    debugDefs,
    module Debug.Trace,
)
where

import Control.Lens (view)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Word (Word8)
import Debug.Trace
import Polysemy (Member, Sem)
import Polysemy.State (State, get, gets)
import Text.Hex qualified as H
import Witch (from)

import Data.BULK.Lens (definitions)
import Data.BULK.Types

class Debug a where
    debug :: a -> String
    default debug :: (Show a) => a -> String
    debug = show

traceD :: (Debug a) => a -> b -> b
traceD thing = trace $ debug thing

traceIdD :: (Debug a) => a -> a
traceIdD = traceWith debug

traceDM :: (Debug a, Monad m) => a -> m ()
traceDM = traceM . debug

instance Debug BULK where
    debug Nil = "nil"
    debug (Form content) = "( " ++ unwords (map debug content) ++ " )"
    debug (Array bs) = [i|[#{debug bs}]|]
    debug (Reference name Nothing) = debug name
    debug (Reference (Name ns _) (Just name)) = [i|#{debug ns}:#{name}|]

instance Debug BS.ByteString where
    debug = from . H.encodeHex

instance Debug LBS.ByteString where
    debug = from . H.lazilyEncodeHex

instance Debug NamespaceID where
    debug CoreNS = "{core}"
    debug TempNS = "{!}"
    debug (UnassociatedNS num) = [i|{#{num}}|]
    debug (MatchEq bulk) = [i|== #{debug bulk}|]
    debug (MatchNamePrefix num bs) = [i|{~???:#{num}/#{take 6 $ debug bs}}|]
    debug (MatchQualifiedNamePrefix (Name (MatchNamePrefix _ hbs) hname) bs) = [i|{~#{take 3 $ debug hbs}:#{hname}/#{take 6 $ debug bs}}|]
    debug (MatchQualifiedNamePrefix (Name _id hname) bs) = [i|{~~~~:#{hname}/#{take 6 $ debug bs}}|]

instance Debug Name where
    debug (Name ns num) = [i|#{debug ns}:#{num}|]

instance Debug Namespace where
    debug Namespace{..} = [i|#{mnemonic}(#{length names}, #{debug matchID}, #{map debug names})|]

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

instance Debug Word8

instance {-# OVERLAPPING #-} Debug [Char] where
    debug = id

instance Debug T.Text where
    debug = T.unpack

instance (Debug v) => Debug (S.Set v) where
    debug = show . map debug . S.toList

instance Debug Scope where
    debug Scope{..} =
        [i|ANSS: #{debug _associatedNamespaces}, KNSS: #{debug _knownNamespaces}, LNSS: #{debug _lastingNamespaces}, KPKG: #{debug _knownPackages}
Defs: #{debug _definitions}|]

instance Debug Value where
    debug SelfEval = "=="
    debug (Expression bulk) = [i|=#{debug bulk}|]
    debug (Digest digest) = [i|=~#{digest}|]
    debug (LazyFunction f) = [i|=#{f}()|]

debugState :: (Member (State s) r, Debug s) => Sem r ()
debugState = get >>= traceM . debug

newtype Definition = Definition (Name, Value)

instance Debug Definition where
    debug (Definition (name, value)) = debug name <> debug value

debugDefs :: (Member (State Scope) r) => [Int] -> [Namespace] -> Sem r ()
debugDefs markers nss = do
    defs <- gets $ M.toList . view definitions
    traceDM $ map Definition $ withMarkers defs ++ withNSS defs
  where
    withMarkers = filter hasMarker
    hasMarker (Name (UnassociatedNS m) _name, _value) = m `elem` markers
    hasMarker _def = False
    withNSS = filter hasNS
    hasNS (Name nsid _name, _value) = nsid `elem` map (.matchID) nss
