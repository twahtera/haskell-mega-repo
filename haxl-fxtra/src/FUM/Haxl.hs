{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module FUM.Haxl
  ( fetchUsers
  , fetchList
  , initDataSource
  , FumRequest(..)
  ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson              (FromJSON)
import Data.Hashable           (Hashable (..))
import Data.Typeable           (Typeable)
import Data.Vector             (Vector)
import Haxl.Core
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified FUM

data FumRequest a where
    FumRequest :: FromJSON a => FUM.FUM a -> FumRequest a

deriving instance Show (FumRequest a)
deriving instance Typeable FumRequest
deriving instance Eq (FumRequest a)

instance Show1 FumRequest where show1 = show

instance Hashable (FumRequest a) where
    hashWithSalt salt (FumRequest req) =
        salt `hashWithSalt` req

fetchUsers :: GenHaxl u (Vector FUM.User)
fetchUsers = dataFetch $ FumRequest FUM.fumUsersR

fetchList :: FUM.ListName -> GenHaxl u (Vector FUM.User)
fetchList = dataFetch . FumRequest . FUM.fumListR

instance StateKey FumRequest where
    data State FumRequest = FumState Manager FUM.AuthToken FUM.BaseUrl

initDataSource :: FUM.AuthToken          -- ^ Authentication token
               -> FUM.BaseUrl            -- ^ Base url
               -> IO (State FumRequest)
initDataSource token baseUrl = do
    mgr <- newManager tlsManagerSettings
    pure (FumState mgr token baseUrl)

instance DataSourceName FumRequest where
    dataSourceName _ = "FumDataSource"

instance DataSource u FumRequest where
    fetch (FumState mgr token baseUrl) _flags _userEnv blockedFetches =
        SyncFetch $ batchFetch mgr token baseUrl blockedFetches

batchFetch :: Manager
           -> FUM.AuthToken
           -> FUM.BaseUrl
           -> [BlockedFetch FumRequest]
           -> IO ()
batchFetch mgr token baseUrl = mapM_ (doFetch mgr token baseUrl)

doFetch :: Manager
        -> FUM.AuthToken
        -> FUM.BaseUrl
        -> BlockedFetch FumRequest
        -> IO ()
doFetch mgr token baseUrl (BlockedFetch (FumRequest req) v) =
    FUM.executeRequest mgr token baseUrl req >>= putSuccess v
