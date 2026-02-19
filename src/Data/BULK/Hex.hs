{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.BULK.Hex (hex) where

import Data.ByteString.Lazy qualified as LBS
import Data.Maybe qualified as M
import Data.Text qualified as T
import Language.Haskell.TH (Exp, appE, stringE)
import Language.Haskell.TH.QuasiQuoter (Q, QuasiQuoter (quoteExp), namedDefaultQuasiQuoter)
import Text.Hex qualified as H

hex :: QuasiQuoter
hex = (namedDefaultQuasiQuoter "hex"){quoteExp = hexStringToLazyBS}

hexStringToLazyBS :: String -> Q Exp
hexStringToLazyBS str =
    case H.decodeHex $ T.pack str of
        Just _ -> appE [|LBS.fromStrict . M.fromJust . H.decodeHex . T.pack|] $ stringE str
        Nothing -> fail "not hex"
