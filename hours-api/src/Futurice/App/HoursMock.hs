{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.HoursMock (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant
import Servant

import Futurice.App.HoursApi.API
import Futurice.App.HoursMock.Config (Config (..))
import Futurice.App.HoursMock.Ctx
import Futurice.App.HoursMock.Logic
       (entryDeleteEndpoint, entryEndpoint, entryIdEndpoint, hoursEndpoint,
       projectEndpoint, userEndpoint)

server :: Ctx -> Server FutuhoursAPI
server ctx = pure "This is futuhours mock api"
    :<|> projectEndpoint ctx
    :<|> userEndpoint ctx
    :<|> hoursEndpoint ctx
    :<|> entryEndpoint ctx
    :<|> entryIdEndpoint ctx
    :<|> entryDeleteEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "Futuhours MOCK api"
    & serverDescription     .~ "Is it real?"
    & serverApp futuhoursApi .~ server
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverEnvPfx          .~ "FUTUHOURSMOCK"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx _ _ _ = do
        pure (Ctx, [])
