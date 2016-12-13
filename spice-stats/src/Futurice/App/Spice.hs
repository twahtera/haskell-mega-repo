{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Spice (defaultMain) where

import Futurice.Prelude
import Prelude          ()

import Network.HTTP.Client        (Manager, newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Servant
import Futurice.Servant

import Futurice.App.Spice.Config
import Futurice.App.Spice.Logic
import Futurice.App.Spice.Server.API

type Ctx = (DynMapCache, Logger, Manager, Config)

serveSpiceStats :: Ctx -> ExceptT ServantErr IO Stats
serveSpiceStats (cache, logger, mgr, cfg) = lift $ cachedIO logger cache 600 () $ do
    msgs <- fetchMessagesLoop (cfgFdOrg cfg) (cfgFdFlow cfg) (cfgFdAuth cfg) mgr
    spiceStats mgr msgs (cfgGhAuth cfg)

-- | API server
server :: Ctx -> Server SpiceAPI
server ctx = pure "Hello from spice stats app"
    :<|> serveSpiceStats ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName              .~ "Spice stats API"
    & serverDescription       .~ "Open source contribution stats"
    & serverColour            .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    & serverApp spiceStatsApi .~ server
    & serverEnvPfx            .~ "SPICESTATS"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx cfg logger cache = do
        mgr <- newManager tlsManagerSettings
        return (cache, logger, mgr, cfg)
