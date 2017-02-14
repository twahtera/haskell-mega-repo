{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.HoursApi.API where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant (SSOUser)
import Servant

import Futurice.App.HoursApi.Types

import qualified PlanMill as PM

type FutuhoursV1API =
    "projects" :> SSOUser :> Get '[JSON] (Vector Project)
    :<|> "user" :> SSOUser :> Get '[JSON] User
    :<|> "hours" :> SSOUser :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> Get '[JSON] (HoursResponse)
    :<|> "entry" :> SSOUser :> ReqBody '[JSON] EntryUpdate :> Post '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" PM.TimereportId :> ReqBody '[JSON] EntryUpdate :> Put '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" PM.TimereportId :> Delete '[JSON] EntryUpdateResponse

type FutuhoursAPI = Get '[JSON] Text
    :<|> "api" :> "v1" :> FutuhoursV1API

futuhoursApi :: Proxy FutuhoursAPI
futuhoursApi = Proxy
