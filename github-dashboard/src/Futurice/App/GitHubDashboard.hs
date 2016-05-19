{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.GitHubDashboard (defaultMain) where

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
import Data.Pool                   (Pool, createPool, withResource)

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified Database.PostgreSQL.Simple    as Postgres

import Futurice.App.GitHubDashboard.Config
import Futurice.App.GitHubDashboard.Logic
import Futurice.App.GitHubDashboard.Server.API

-- | TODO: use reader monad
type Ctx = (DynMapCache, Manager, Pool Postgres.Connection, Config)

servePullrequests :: Ctx -> ExceptT ServantErr IO PullRequests 
servePullrequests (cache, mgr, connPool, cfg) =
    lift $ cachedIO cache 600 () $ withResource connPool $ \conn ->
    pullrequests mgr conn (cfgGhAuth cfg)

serveIssues :: Ctx -> ExceptT ServantErr IO Issues 
serveIssues (cache, mgr, connPool, cfg) =
    lift $ cachedIO cache 600 () $ withResource connPool $ \conn ->
    issues mgr conn (cfgGhAuth cfg)

-- | API server
server :: Ctx -> Server DashboardAPI 
server ctx = pure "Hello from github dashboard app"
    :<|> servePullrequests ctx
    :<|> serveIssues ctx

-- | Server with docs and cache and status
server' :: DynMapCache -> String -> Ctx -> Server DashboardAPI'
server' cache versionHash ctx = serverAvatarApi cache versionHash (server ctx)

-- | Wai application
app :: DynMapCache -> String -> Ctx -> Application
app cache versionHash ctx = serve avatarApi' (server' cache versionHash ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, github-dashaboard-server is alive"
    cfg@Config {..} <- getConfig
    mgr <- newManager tlsManagerSettings
    cache <- DynMap.newIO
    postgresPool <- createPool (Postgres.connect cfgPostgresConnInfo) Postgres.close 1 10 5
    let ctx = (cache, mgr, postgresPool, cfg)
    let app' = app cache $(gitHash) ctx
    hPutStrLn stderr $ "Starting web server in port " ++ show cfgPort
    Warp.run cfgPort app'

