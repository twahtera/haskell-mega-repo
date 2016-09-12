{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Data source for raw http requests
module Haxl.Extra.HttpDataSource
    ( httpGet
    , initDataSource
    , HttpRequest
    ) where

import Futurice.Prelude

import qualified Data.ByteString.Lazy as LBS

import           Haxl.Core
import qualified Haxl.Extra.IODataSource as IODS
import           Network.HTTP.Client     (Manager, httpLbs, newManager,
                                          parseUrlThrow, responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

newtype HttpTag = HttpTag String
    deriving (Eq, Show, Generic, Typeable)

instance Hashable HttpTag

type HttpRequest = IODS.GenIORequest HttpTag

httpGet :: String       -- ^ URL
        -> GenHaxl u LBS.ByteString
httpGet url = IODS.ioAction (HttpTag url) $ \mgr -> do
    req <- parseUrlThrow url
    res <- httpLbs req mgr
    pure (responseBody res)

initDataSource :: IO (State HttpRequest)
initDataSource = IODS.initDataSource 10 =<< newManager tlsManagerSettings

instance IODS.IODataSourceTag HttpTag where
    type IOEnv HttpTag = Manager
    ioSourceName _ = "HttpDataSource"
