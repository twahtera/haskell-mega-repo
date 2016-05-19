{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module FUM (
    -- * Actions
    fumUsers,
    fumList,
    -- * Requests
    fumUsersR,
    fumListR,
    -- * Legacy Methods
    fetchUsers,
    fetchList,
    -- * Types
    module FUM.Types,
    module FUM.Request,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Monad.Http   (runHttpT)
import Control.Monad.Reader (runReaderT)
import Network.HTTP.Client  (Manager)

import Control.Monad.FUM
import FUM.Request
import FUM.Types

-------------------------------------------------------------------------------
-- Requests
-------------------------------------------------------------------------------

fumUsersR :: FUM (Vector User)
fumUsersR = FumPagedGet "users/"

fumListR :: ListName -> FUM (Vector User)
fumListR (ListName listName) =
    FumGet $ "list/" <> listName ^. from packed <> "/"

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

fumUsers :: MonadFUM m => m (Vector User)
fumUsers = fumAction fumUsersR

fumList :: MonadFUM m => ListName -> m (Vector User)
fumList = fumAction . fumListR

-------------------------------------------------------------------------------
-- Legacy
-------------------------------------------------------------------------------

fetchUsers :: Manager
           -> AuthToken
           -> BaseUrl
           -> IO (Vector User)
fetchUsers mgr token burl =
    flip runHttpT mgr $ flip runReaderT cfg $ evalFUM fumUsersR
  where cfg = Cfg burl token

fetchList :: Manager
          -> AuthToken
          -> BaseUrl
          -> ListName
          -> IO (Vector User)
fetchList mgr token burl listname =
    flip runHttpT mgr $ flip runReaderT cfg $ evalFUM (fumListR listname)
  where cfg = Cfg burl token


