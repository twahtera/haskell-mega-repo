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
import Futurice.App.HoursApi.Logic

import Futurice.App.HoursMock.Config (Config (..))
import Futurice.App.HoursMock.Ctx
import Futurice.App.HoursMock.Monad

server :: Ctx -> Server FutuhoursAPI
server ctx = pure "This is futuhours mock api"
    :<|> (\_        -> runHours ctx projectEndpoint)
    :<|> (\_        -> runHours ctx userEndpoint)
    :<|> (\_ a b    -> runHours ctx (hoursEndpoint a b))
    :<|> (\_ eu     -> runHours ctx (entryEndpoint eu))
    :<|> (\_ eid eu -> runHours ctx (entryEditEndpoint eid eu))
    :<|> (\_ eid    -> runHours ctx (entryDeleteEndpoint eid))
    :<|> (\_        -> runHours ctx preferencesEndpoint)
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
        ctx <- newCtx
        pure (ctx, [])
