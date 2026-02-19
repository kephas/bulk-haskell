{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BULK.Debug (
    Debug (..),
    detrace,
    detraceId,
    detraceM,
    detraceState,
    module Debug.Trace,
)
where

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
import Polysemy.State (State, get)
import Text.Hex qualified as H
import Witch (from)

import Data.BULK.Types
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

class Debug a where
    debug :: a -> String
    default debug :: (Show a) => a -> String
    debug = show

detrace :: (Debug a) => a -> b -> b
detrace thing = trace $ debug thing

detraceId :: (Debug a) => a -> a
detraceId = traceWith debug

detraceM :: (Debug a, Monad m) => a -> m ()
detraceM = traceM . debug

instance Debug BULK where
    debug Nil = "nil"
    debug (Form content) = "( " ++ unwords (map debug content) ++ " )"
    debug (Array bs) = [i|[#{debug bs}]|]
    debug (Reference ref) = debug ref

instance Debug BS.ByteString where
    debug = from . H.encodeHex

instance Debug LBS.ByteString where
    debug = from . H.lazilyEncodeHex

instance Debug NamespaceID where
    debug CoreNS = "{core}"
    debug TempNS = "{!}"
    debug (UnassociatedNS num) = [i|{#{num}}|]
    debug (MatchEq bulk) = [i|== #{debug bulk}|]
    debug (MatchNamePrefix num bs) = [i|{#{take 6 $ debug bs}~~<<:#{num}}|]
    debug (MatchQualifiedNamePrefix (Ref (MatchNamePrefix _ hbs) Name{marker}) bs) = [i|{#{take 6 $ debug bs}~#{take 3 $ debug hbs}:#{debug marker}}|]
    debug (MatchQualifiedNamePrefix (Ref _id hname) bs) = [i|{~~~~:#{hname}/#{take 6 $ debug bs}}|]

instance Debug Ref where
    debug (Ref ns name) = [i|#{debug ns}:#{shortName name}|]

shortName :: Name -> String
shortName Name{marker, mnemonic = Nothing} = debug marker
shortName Name{mnemonic = Just name} = debug name

instance Debug Namespace where
    debug Namespace{..} = [i|#{mnemonic}(#{length names};#{debug matchID} ;; #{debug names})|]

instance Debug Name where
    debug Name{marker, mnemonic, value} = [i|#{marker}:#{fromMaybe "" mnemonic}#{debug value}|]

instance Debug Package where
    debug Package{..} = [i|{pkg (#{debug matchID}) #{debug nsIDs}|]

instance Debug Context where
    debug Context{..} = [i|{{NSS: #{debug namespaces} PKGS: #{debug packages}}}|]

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
    debug = debug . map (bimap debug debug) . M.toList

instance Debug Int

instance Debug Word8

instance Debug Int64

instance {-# OVERLAPPING #-} Debug [Char] where
    debug = id

instance Debug T.Text where
    debug = T.unpack

instance (Debug v) => Debug (S.Set v) where
    debug = debug . map debug . S.toList

instance Debug Scope where
    debug Scope{..} =
        [i|ANSS: #{debug _associatedNamespaces}, KNSS: #{debug _knownNamespaces}, LNSS: #{debug _lastingNamespaces}, KPKG: #{debug _knownPackages}|]

instance Debug Value where
    debug SelfEval = "=="
    debug (Expression bulk) = [i|=#{debug bulk}|]
    debug (Digest digest) = [i|=~#{digest}|]
    debug (LazyFunction f) = [i|=#{f}()|]

instance Debug Warning where
    debug (Warning w) = w

detraceState :: (Member (State s) r, Debug s) => Sem r ()
detraceState = get >>= traceM . debug
