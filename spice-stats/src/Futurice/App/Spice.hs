{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Spice (defaultMain) where

import Futurice.Prelude
import Prelude          ()

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Network.HTTP.Client        (Manager, newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Network.Wai
import Servant
import System.IO                  (hPutStrLn, stderr)
import Futurice.Servant

import qualified Network.Wai.Handler.Warp      as Warp

import Futurice.App.Spice.Config
import Futurice.App.Spice.Logic
import Futurice.App.Spice.Server.API

type Ctx = (DynMapCache, Manager, Config)

serveSpiceStats :: Ctx -> ExceptT ServantErr IO Stats
serveSpiceStats (cache, mgr, cfg) = lift $ cachedIO cache 600 () $ do
    msgs <- fetchMessagesLoop (cfgFdOrg cfg) (cfgFdFlow cfg) (cfgFdAuth cfg) mgr
    spiceStats mgr msgs (cfgGhAuth cfg)

-- | API server
server :: Ctx -> Server SpiceAPI
server ctx = pure "Hello from spice stats app"
    :<|> serveSpiceStats ctx

-- | Server with docs and cache and status
server' :: DynMapCache -> Ctx -> Server SpiceAPI'
server' cache ctx = futuriceServer
    "Spice stats API"
    "Open source contribution stats"
    cache spiceStatsApi (server ctx)

-- | Wai application
app :: DynMapCache -> Ctx -> Application
app cache ctx = serve spiceStatsApi' (server' cache  ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, spice-stats-server is alive"
    cfg <- getConfig
    mgr <- newManager tlsManagerSettings
    cache <- newDynMapCache 
    let ctx = (cache, mgr, cfg)
    let app' = app cache ctx
    hPutStrLn stderr $ "Starting web server in port " ++ show (cfgPort cfg)
    Warp.run (cfgPort cfg) app'

