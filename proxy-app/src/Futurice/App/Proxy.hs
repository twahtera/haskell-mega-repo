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
import Data.Pool                       (createPool, withResource)
import Data.Text.Encoding              (decodeLatin1)
import Futurice.Servant
import Network.HTTP.Client             (newManager)
import Network.HTTP.Client.TLS         (tlsManagerSettings)
import Network.Wai ()
import Network.Wai.Middleware.HttpAuth (basicAuth)
import PlanMill.Types.Query            (SomeQuery, SomeResponse)
import Servant
import Servant.Binary.Tagged           (BINARYTAGGED)
import Servant.Client
import Servant.Proxy
import System.IO                       (hPutStrLn, stderr)

import qualified Database.PostgreSQL.Simple as Postgres

import Futurice.App.Proxy.Config
import Futurice.App.Proxy.Ctx
import Futurice.App.Reports.MissingHours (MissingHoursReport)

-------------------------------------------------------------------------------
-- Services
-------------------------------------------------------------------------------

data ReportsAppService
data PlanmillProxyService

instance HasClientBaseurl Ctx ReportsAppService where
    clientBaseurl _ = lens ctxReportsAppBaseurl $ \ctx x ->
        ctx { ctxReportsAppBaseurl = x }

instance HasClientBaseurl Ctx PlanmillProxyService where
    clientBaseurl _ = lens ctxPlanmillProxyBaseurl $ \ctx x ->
        ctx { ctxPlanmillProxyBaseurl = x }

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

type MissingReportsEndpoint = ProxyPair
    ("futuhours" :> "reports" :> "missinghours" :> Get '[CSV, JSON] MissingHoursReport)
    ReportsAppService
    ("missing-hours" :> Get '[JSON] MissingHoursReport)

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

proxyAPI :: Proxy ProxyAPI
proxyAPI = Proxy

-------------------------------------------------------------------------------
-- WAI/startup
------------------------------------------------------------------------------

server :: Ctx -> Server ProxyAPI
server ctx = pure "P-R-O-X-Y"
    :<|> makeProxy (Proxy :: Proxy MissingReportsEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy PlanmillProxyEndpoint) ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName         .~ "Proxy-app management"
    & serverDescription  .~ "Audit log"
    & serverColour       .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC3))
    & serverApp proxyAPI .~ server
    & serverMiddleware   .~ (\ctx -> basicAuth (checkCreds ctx) "P-R-O-X-Y")
    & serverEnvPfx       .~ "PROXYMGMT"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx Config {..} _logger _cache = do
        mgr                  <- newManager tlsManagerSettings
        reportsAppBaseurl    <- parseBaseUrl cfgReportsAppBaseurl
        planmillProxyBaseUrl <- parseBaseUrl cfgPlanmillProxyBaseurl
        postgresPool         <- createPool
            (Postgres.connect cfgPostgresConnInfo)
            Postgres.close
            1 10 5
        pure $ Ctx mgr postgresPool reportsAppBaseurl planmillProxyBaseUrl

checkCreds :: Ctx -> ByteString -> ByteString -> IO Bool
checkCreds ctx u p = withResource (ctxPostgresPool ctx) $ \conn -> do
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
