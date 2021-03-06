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
import Data.Reflection                 (Given (..), give)
import Data.Text.Encoding              (decodeLatin1)
import Futurice.Servant
import Network.Wai                     (Request, rawPathInfo)
import Network.Wai.Middleware.HttpAuth (basicAuth')
import Servant
import Servant.Binary.Tagged           (BINARYTAGGED)
import Servant.Client
import Servant.Common.Req              (Req (headers))
import Servant.Proxy
import System.IO                       (hPutStrLn, stderr)

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM
import qualified Futurice.GitHub            as GH (SomeRequest, SomeResponse)
import qualified PlanMill.Types.Query       as PM (SomeQuery, SomeResponse)

import Futurice.App.Proxy.Config
import Futurice.App.Proxy.Ctx
import Futurice.App.Reports.MissingHours (MissingHoursReport)

-------------------------------------------------------------------------------
-- Services
-------------------------------------------------------------------------------

data ReportsAppService
data PlanmillProxyService
data GithubProxyService
data FumService

instance HasClientBaseurl Ctx ReportsAppService where
    clientBaseurl _ = lens ctxReportsAppBaseurl $ \ctx x ->
        ctx { ctxReportsAppBaseurl = x }

instance HasClientBaseurl Ctx PlanmillProxyService where
    clientBaseurl _ = lens ctxPlanmillProxyBaseurl $ \ctx x ->
        ctx { ctxPlanmillProxyBaseurl = x }

instance HasClientBaseurl Ctx GithubProxyService where
    clientBaseurl _ = lens ctxGithubProxyBaseurl $ \ctx x ->
        ctx { ctxGithubProxyBaseurl = x }

instance HasClientBaseurl Ctx FumService where
    clientBaseurl _ = lens ctxFumBaseurl $ \ctx x ->
        ctx { ctxFumBaseurl = x }

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- Reports
type MissingReportsEndpoint = ProxyPair
    ("futuhours" :> "reports" :> "missinghours" :> Get '[CSV, JSON] MissingHoursReport)
    ReportsAppService
    ("missing-hours" :> Get '[JSON] MissingHoursReport)

-- Planmill
-- TODO: we actually decode/encode when proxying.
-- Is this bad?
type PlanmillProxyEndpoint' =
    ReqBody '[JSON] [PM.SomeQuery] :> Post '[BINARYTAGGED] [Either Text PM.SomeResponse]
type PlanmillProxyEndpoint = ProxyPair
    ("planmill-haxl" :> PlanmillProxyEndpoint')
    PlanmillProxyService
    ("planmill-haxl" :> PlanmillProxyEndpoint')

-- Github
type GithubProxyEndpoint' =
    ReqBody '[JSON] [GH.SomeRequest] :> Post '[BINARYTAGGED] [Either Text GH.SomeResponse]

type GithubProxyEndpoint = ProxyPair
    ("github-haxl" :> GithubProxyEndpoint')
    GithubProxyService
    ("github-haxl" :> GithubProxyEndpoint')

-- Fum
type FumEmployeesEndpoint = ProxyPair
    ("fum" :> "list" :> "employees" :> Get '[JSON] Value)
    FumService
    ("list" :> "employees" :> Get '[JSON] Value)

type FumUserEndpoint = ProxyPair
    ("fum" :> "users" :> Capture "uid" Text :> Get '[JSON] Value)
    FumService
    (WithFumAuthToken :> "users" :> Capture "uid" Text :> Get '[JSON] Value)

-- | Whole proxy definition
type ProxyDefinition =
    '[ MissingReportsEndpoint
    , PlanmillProxyEndpoint
    , GithubProxyEndpoint
    , FumEmployeesEndpoint
    , FumUserEndpoint
    ]

type ProxyAPI  = Get '[JSON] Text :<|> ProxyServer ProxyDefinition

proxyAPI :: Proxy ProxyAPI
proxyAPI = Proxy

-------------------------------------------------------------------------------
-- Fum AuthToken "hack"
-------------------------------------------------------------------------------

-- We use @reflection@ to 'give' 'FUM.AuthToken'.

data WithFumAuthToken
instance (Given FUM.AuthToken, HasClient api)
    => HasClient (WithFumAuthToken :> api)
  where
    type Client (WithFumAuthToken :> api) = Client api
    clientWithRoute _ req = clientWithRoute (Proxy :: Proxy api) req'
      where
        req' = req { headers = ("Authorization", "Token " <> given ^. FUM.getAuthToken) : headers req }

-------------------------------------------------------------------------------
-- WAI/startup
------------------------------------------------------------------------------

server :: Ctx -> Server ProxyAPI
server ctx = give (ctxFumAuthToken ctx) $ pure "P-R-O-X-Y"
    :<|> makeProxy (Proxy :: Proxy MissingReportsEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy PlanmillProxyEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy GithubProxyEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy FumEmployeesEndpoint) ctx
    :<|> makeProxy (Proxy :: Proxy FumUserEndpoint) ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName         .~ "Proxy-app"
    & serverDescription  .~ "Proxy from the outer space"
    & serverColour       .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC3))
    & serverApp proxyAPI .~ server
    & serverMiddleware   .~ (\ctx -> basicAuth' (checkCreds ctx) "P-R-O-X-Y")
    & serverEnvPfx       .~ "PROXYMGMT"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx Config {..} _logger _cache = do
        mgr                  <- newManager tlsManagerSettings
        reportsAppBaseurl    <- parseBaseUrl cfgReportsAppBaseurl
        planmillProxyBaseUrl <- parseBaseUrl cfgPlanmillProxyBaseurl
        githubProxyBaseurl   <- parseBaseUrl cfgGithubProxyBaseurl
        fumBaseurl           <- parseBaseUrl cfgFumBaseurl
        postgresPool         <- createPool
            (Postgres.connect cfgPostgresConnInfo)
            Postgres.close
            1 10 5
        pure $ flip (,) [] $ Ctx
            { ctxManager              = mgr
            , ctxPostgresPool         = postgresPool
            , ctxReportsAppBaseurl    = reportsAppBaseurl
            , ctxPlanmillProxyBaseurl = planmillProxyBaseUrl
            , ctxGithubProxyBaseurl   = githubProxyBaseurl
            , ctxFumBaseurl           = fumBaseurl
            , ctxFumAuthToken         = cfgFumAuthToken
            }

checkCreds :: Ctx -> Request -> ByteString -> ByteString -> IO Bool
checkCreds ctx req u p = withResource (ctxPostgresPool ctx) $ \conn -> do
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
            let endpoint = decodeLatin1 $ rawPathInfo req
            _ <- Postgres.execute conn
                "insert into proxyapp.accesslog (username, endpoint) values (?, ?);"
                (u', endpoint)
            pure True
