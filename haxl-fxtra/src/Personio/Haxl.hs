{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module Personio.Haxl (
    request,
    initDataSource,
    PersonioRequest(..),
    ) where

import Futurice.Prelude

import Data.Aeson (FromJSON)
import Haxl.Core

import qualified Personio

data PersonioRequest a where
    PersonioRequest :: FromJSON a => Personio.PersonioReq a -> PersonioRequest a

deriving instance Show (PersonioRequest a)
deriving instance Typeable PersonioRequest
deriving instance Eq (PersonioRequest a)

instance Haxl.Core.ShowP PersonioRequest where showp = show

instance Hashable (PersonioRequest a) where
    hashWithSalt salt (PersonioRequest req) =
        salt `hashWithSalt` req

request
    :: (Show a, Typeable a, FromJSON a)
    => Personio.PersonioReq a -> GenHaxl u a
request = dataFetch . PersonioRequest

instance StateKey PersonioRequest where
    data State PersonioRequest = PersonioState Manager Logger Personio.Cfg

initDataSource
    :: Manager               -- ^ HTTP manager
    -> Logger                -- ^ Logger
    -> Personio.Cfg          -- ^ Configuration
    -> State PersonioRequest
initDataSource = PersonioState

instance DataSourceName PersonioRequest where
    dataSourceName _ = "PersonioDataSource"

instance DataSource u PersonioRequest where
    fetch (PersonioState mgr lgr cfg) _flags _userEnv blockedFetches =
        SyncFetch $ batchFetch mgr lgr cfg blockedFetches

batchFetch
    :: Manager
    -> Logger
    -> Personio.Cfg
    -> [BlockedFetch PersonioRequest]
    -> IO ()
batchFetch mgr lgr cfg = traverse_ (doFetch mgr lgr cfg)

doFetch
    :: Manager
    -> Logger
    -> Personio.Cfg
    -> BlockedFetch PersonioRequest
    -> IO ()
doFetch mgr lgr cfg (BlockedFetch (PersonioRequest req) v) =
    Personio.evalPersonioReqIO mgr lgr cfg req >>= putSuccess v
