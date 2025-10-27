{-# LANGUAGE TypeOperators #-}

module Vary.Extra where

import Data.Bifunctor (first)
import Vary
import Vary.VEither

fromEither1 :: (e :| errs) => Either e a -> VEither errs a
fromEither1 = fromEither1Map id

fromEither1Map :: (e2 :| errs) => (e1 -> e2) -> Either e1 a -> VEither errs a
fromEither1Map mapError = fromEither . first (from . mapError)

veitherToMaybe :: VEither errs a -> Maybe a
veitherToMaybe = veither (const Nothing) Just

fromLeft1 :: (Show err, Show a) => err -> VEither '[err] a
fromLeft1 = fromLeft

isVLeft :: VEither errs a -> Bool
isVLeft (VLeft _) = True
isVLeft (VRight _) = False
