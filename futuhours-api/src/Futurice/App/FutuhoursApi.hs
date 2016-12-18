{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursApi (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant
import Servant

import Futurice.App.FutuhoursApi.API
import Futurice.App.FutuhoursApi.Config (Config (..))
import Futurice.App.FutuhoursApi.Ctx
import Futurice.App.FutuhoursApi.Logic
       (entryDeleteEndpoint, entryEndpoint, entryIdEndpoint, hoursEndpoint,
       projectEndpoint, userEndpoint)

server :: Ctx -> Server FutuhoursAPI
server ctx = pure "This is futuhours api"
    :<|> projectEndpoint ctx
    :<|> userEndpoint ctx
    :<|> hoursEndpoint ctx
    :<|> entryEndpoint ctx
    :<|> entryIdEndpoint ctx
    :<|> entryDeleteEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "Futuhours API"
    & serverDescription     .~ "Here we mark hours"
    & serverApp futuhoursApi .~ server
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverEnvPfx          .~ "FUTUHOURSAPI"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx _ _ _ = do
        pure Ctx
