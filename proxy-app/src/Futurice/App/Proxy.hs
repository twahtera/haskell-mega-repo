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
import Data.Tagged                     (tagWith)
import Data.Text.Encoding              (decodeLatin1)
import Database.PostgreSQL.Simple      (Connection)
import Futurice.EnvConfig              (getConfig)
import Futurice.Servant
import Network.HTTP.Client             (Manager, newManager)
import Network.HTTP.Client.TLS         (tlsManagerSettings)
import Network.Wai ()
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Servant
import Servant.Client
import Servant.CSV.Cassava             (CSV)
import Servant.Proxy
import System.IO                       (hPutStrLn, stderr)

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Network.Wai.Handler.Warp   as Warp

import Futurice.App.FutuHours.Types (MissingHoursReport)
import Futurice.App.Proxy.Config

-- | Context type, holds http manager and baseurl configurations
data Ctx = Ctx
    { ctxManager          :: !Manager
    , ctxFutuhoursBaseurl :: !BaseUrl
    }

-- | Services we proxy to
data FutuhoursApiService

type MissingReportsEndpoint = ProxyPair
    ("futuhours" :> "reports" :> "missinghours" :> Get '[CSV, JSON] MissingHoursReport)
    FutuhoursApiService
    ("reports" :> "missinghours" :> Get '[JSON] MissingHoursReport)

-- | Whole proxy definition
type ProxyDefinition =
    '[ MissingReportsEndpoint
    ]

type ProxyAPI  = Get '[JSON] Text :<|> ProxyServer ProxyDefinition
type ProxyAPI' = FuturiceAPI ProxyAPI ('FutuAccent 'AF3 'AC3)

proxyAPI :: Proxy ProxyAPI
proxyAPI = Proxy

proxyAPI' :: Proxy ProxyAPI'
proxyAPI' = Proxy

-------------------------------------------------------------------------------
-- WAI/startup
-------------------------------------------------------------------------------

server :: Ctx -> Server ProxyAPI
server Ctx {..} = pure "P-R-O-X-Y"
    :<|> makeProxy (Proxy :: Proxy MissingReportsEndpoint) futuhoursEnv
  where
    futuhoursEnv = tagWith
        (Proxy :: Proxy FutuhoursApiService)
        (ClientEnv ctxManager ctxFutuhoursBaseurl)

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
    Config{..} <- getConfig
    mgr <- newManager tlsManagerSettings
    cache <- newDynMapCache
    futuhoursBaseurl <- parseBaseUrl cfgFutuhoursBaseurl
    postgresPool <- createPool
        (Postgres.connect cfgPostgresConnInfo)
        Postgres.close
        1 10 5
    let ctx = Ctx mgr futuhoursBaseurl
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
