{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.Proxy (
    defaultMain,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.ByteString                 (ByteString)
import Data.Pool                       (Pool, createPool, withResource)
import Data.Text.Encoding              (decodeLatin1)
import Database.PostgreSQL.Simple      (Connection)
import Futurice.EnvConfig              (getConfig)
import Futurice.Servant
import Network.HTTP.Client             (newManager)
import Network.HTTP.Client.TLS         (tlsManagerSettings)
import Network.Wai ()
import Network.Wai.Middleware.HttpAuth (basicAuth)
import PlanMill.Types.Query            (SomeQuery, SomeResponse)
import Servant
import Servant.Binary.Tagged           (BINARYTAGGED)
import Servant.Client
import Servant.CSV.Cassava             (CSV)
import Servant.Proxy
import System.IO                       (hPutStrLn, stderr)

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Network.Wai.Handler.Warp   as Warp

import Futurice.App.FutuHours.Types (MissingHoursReport)
import Futurice.App.Proxy.Config
import Futurice.App.Proxy.Ctx

-------------------------------------------------------------------------------
-- Services
-------------------------------------------------------------------------------

data FutuhoursApiService
data PlanmillProxyService

instance HasClientBaseurl Ctx FutuhoursApiService where
    clientBaseurl _ = lens ctxFutuhoursBaseurl $ \ctx x ->
        ctx { ctxFutuhoursBaseurl = x }

instance HasClientBaseurl Ctx PlanmillProxyService where
    clientBaseurl _ = lens ctxPlanmillProxyBaseurl $ \ctx x ->
        ctx { ctxPlanmillProxyBaseurl = x }

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

type MissingReportsEndpoint = ProxyPair
    ("futuhours" :> "reports" :> "missinghours" :> Get '[CSV, JSON] MissingHoursReport)
    FutuhoursApiService
    ("reports" :> "missinghours" :> Get '[JSON] MissingHoursReport)

-- TODO: we actually decode/encode when proxying.
-- Is this bad?
type PlanmillProxyEndpoint' =
    ReqBody '[JSON] [SomeQuery] :> Post '[BINARYTAGGED] [Either Text SomeResponse]
type PlanmillProxyEndpoint = ProxyPair
    ("planmill-proxy" :> PlanmillProxyEndpoint')
    PlanmillProxyService
    ("haxl" :> PlanmillProxyEndpoint')

-- | Whole proxy definition
type ProxyDefinition =
    '[ MissingReportsEndpoint
    , PlanmillProxyEndpoint
    ]

type ProxyAPI  = Get '[JSON] Text :<|> ProxyServer ProxyDefinition
type ProxyAPI' = FuturiceAPI ProxyAPI ('FutuAccent 'AF3 'AC3)

proxyAPI :: Proxy ProxyAPI
proxyAPI = Proxy

proxyAPI' :: Proxy ProxyAPI'
proxyAPI' = Proxy

-------------------------------------------------------------------------------
-- WAI/startup
------------------------------------------------------------------------------

server :: Ctx -> Server ProxyAPI
server ctx = pure "P-R-O-X-Y"
    :<|> makeProxy (Proxy :: Proxy MissingReportsEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy PlanmillProxyEndpoint) ctx

-- | Server with docs and cache and status
server' :: DynMapCache -> Ctx -> Server ProxyAPI'
server' cache ctx = futuriceServer
    "Proxy"
    "Proxy into Futurice internal services"
    cache proxyAPI (server ctx)

-- | Wai application
app :: DynMapCache -> Ctx -> Application
app cache ctx = serve proxyAPI' (server' cache ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, proxy-app is alive"
    Config{..}           <- getConfig
    mgr                  <- newManager tlsManagerSettings
    cache                <- newDynMapCache
    futuhoursBaseurl     <- parseBaseUrl cfgFutuhoursBaseurl
    planmillProxyBaseUrl <- parseBaseUrl cfgPlanmillProxyBaseurl
    postgresPool         <- createPool
        (Postgres.connect cfgPostgresConnInfo)
        Postgres.close
        1 10 5
    let ctx = Ctx mgr futuhoursBaseurl planmillProxyBaseUrl
    let app' = basicAuth (checkCreds postgresPool) "P-R-O-X-Y" $ app cache ctx
    hPutStrLn stderr "Starting web server"
    Warp.run cfgPort app'

checkCreds :: Pool Connection -> ByteString -> ByteString -> IO Bool
checkCreds pool u p = withResource pool $ \conn -> do
    let u' = decodeLatin1 u
        p' = decodeLatin1 p
    res <- Postgres.query conn
        "select 1 from proxyapp.credentials where username = ? and passtext = ?;"
        (u', p') :: IO [Postgres.Only Int]
    case res of
        [] -> do
            hPutStrLn stderr $ "Invalid login with: " ++ show (u, p)
            pure False
        _ : _ -> do
            let endpoint = "?" :: Text
            _ <- Postgres.execute conn
                "insert into proxyapp.accesslog (username, endpoint) values (?, ?);"
                (u', endpoint)
            pure True
