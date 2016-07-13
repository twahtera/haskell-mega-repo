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
    executeRequest,
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
import Data.Aeson           (FromJSON)
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

executeRequest
    :: FromJSON a
    => Manager
    -> AuthToken
    -> BaseUrl
    -> FUM a
    -> IO a
executeRequest mgr token burl fum =
    flip runHttpT mgr $ flip runReaderT cfg $ evalFUM fum
  where
    cfg = Cfg burl token

-------------------------------------------------------------------------------
-- Legacy
-------------------------------------------------------------------------------

fetchUsers
    :: Manager
    -> AuthToken
    -> BaseUrl
    -> IO (Vector User)
fetchUsers mgr token burl =
    executeRequest mgr token burl fumUsersR

fetchList
    :: Manager
    -> AuthToken
    -> BaseUrl
    -> ListName
    -> IO (Vector User)
fetchList mgr token burl listname =
    executeRequest mgr token burl $ fumListR listname
