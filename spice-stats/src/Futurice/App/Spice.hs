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

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Network.HTTP.Client        (Manager, newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Servant
import Futurice.Servant

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

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Spice stats API"
    "Open source contribution stats"
    (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    getConfig cfgPort
    spiceStatsApi server futuriceNoMiddleware
    $ \cfg cache -> do
        mgr <- newManager tlsManagerSettings
        return (cache, mgr, cfg)
