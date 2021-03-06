{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuHours.API where

import Futurice.Prelude
import Servant

import Futurice.App.FutuHours.Types

type LegacyFutuhoursAPI =
    --"timereports" :> Capture "fum-id" FUMUsername :> Get '[JSON] (Vector Timereport)
    -- :<|>
    "projects" :> Capture "userid" UserId :> Get '[JSON] (Vector Project)
    :<|> "holidays" :> Get '[JSON] (Envelope ()) -- TODO
    :<|> "users" :> Header "Remote-User" Text :> Get '[JSON] (Envelope User)
    :<|> "hours" :> Header "Remote-User" Text :> QueryParam "day__lte" Day :> QueryParam "day__gte" Day :> Get '[JSON] (Envelope Hour)

type FutuHoursAPI = Get '[PlainText] Text
    :<|> "add-planmill-token" :> Capture "fum-id" FUMUsername :> ReqBody '[JSON] PlanmillApiKey :> Put '[JSON] ()
    :<|> "power" :>
        ( "users" :> Get '[JSON] (Vector PowerUser)
        :<|> "absences" :> QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] (Vector PowerAbsence)
        )
    :<|> "api" :> "v1" :> LegacyFutuhoursAPI

futuhoursAPI :: Proxy FutuHoursAPI
futuhoursAPI = Proxy
