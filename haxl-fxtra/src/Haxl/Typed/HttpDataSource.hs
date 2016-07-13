{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Haxl.Typed.HttpDataSource
    ( httpGet
    , Haxl.initDataSource
    , Haxl.HttpRequest
    ) where

import Futurice.Prelude

import qualified Data.ByteString.Lazy      as LBS
import qualified Haxl.Extra.HttpDataSource as Haxl
import           Haxl.Typed

httpGet
    :: IsElem' Haxl.HttpRequest r (Index Haxl.HttpRequest r)
    => String       -- ^ URL
    -> GenTyHaxl r u LBS.ByteString
httpGet = GenTyHaxl . Haxl.httpGet
