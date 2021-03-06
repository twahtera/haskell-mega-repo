{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursApi.API where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant (SSOUser)
import Servant

import Futurice.App.FutuhoursApi.Types

type FutuhoursV1API =
    "projects" :> SSOUser :> Get '[JSON] (Vector Project)
    :<|> "user" :> SSOUser :> Get '[JSON] User -- TODO: should return logged-in User information
    :<|> "hours" :> SSOUser :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> Get '[JSON] (HoursResponse)
    :<|> "entry" :> SSOUser :> ReqBody '[JSON] EntryUpdate :> Post '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" Int :> ReqBody '[JSON] EntryUpdate :> Put '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" Int :> ReqBody '[JSON] EntryUpdate :> Delete '[JSON] EntryUpdateResponse

type FutuhoursAPI = Get '[JSON] Text
    :<|> "api" :> "v1" :> FutuhoursV1API

futuhoursApi :: Proxy FutuhoursAPI
futuhoursApi = Proxy
