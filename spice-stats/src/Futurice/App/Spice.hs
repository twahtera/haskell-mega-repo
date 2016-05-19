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
import Development.GitRev         (gitHash)
import Network.HTTP.Client        (Manager, newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Network.Wai
import Servant
import Servant.Cache.Class        (DynMapCache, cachedIO)
import System.IO                  (hPutStrLn, stderr)

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Servant.Cache.Internal.DynMap as DynMap

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
server' :: DynMapCache -> String -> Ctx -> Server SpiceAPI'
server' cache versionHash ctx = serverAvatarApi cache versionHash (server ctx)

-- | Wai application
app :: DynMapCache -> String -> Ctx -> Application
app cache versionHash ctx = serve avatarApi' (server' cache versionHash ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, spice-stats-server is alive"
    cfg <- getConfig
    mgr <- newManager tlsManagerSettings
    cache <- DynMap.newIO
    let ctx = (cache, mgr, cfg)
    let app' = app cache $(gitHash) ctx
    hPutStrLn stderr $ "Starting web server in port " ++ show (cfgPort cfg)
    Warp.run (cfgPort cfg) app'

