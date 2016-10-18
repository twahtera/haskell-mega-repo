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
module Futurice.App.Proxy (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Data.ByteString                 (ByteString)
import Data.Pool                       (Pool, createPool, withResource)
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

data Ctx = Ctx
    { ctxManager          :: !Manager
    , ctxFutuhoursBaseurl :: !BaseUrl
    }

makeProxy
    :: forall api.
      ( Proxyable api
      , S (ProxyNamespace api :> ProxiedAPI api) ~ Server (ProxyNamespace api :> ProxiedAPI api)
      , C (ProxiedAPI api) ~ Client (JSONAPI (ProxiedAPI api))
      , HasClient (JSONAPI (ProxiedAPI api))
      )
    => Proxy api -> Ctx -> Server (ProxyNamespace api :> ProxiedAPI api)
makeProxy _ ctx = proxy' p (ClientEnv manager baseurl) (client p')
  where
    baseurl = ctxFutuhoursBaseurl ctx  -- TODO: make a class with Ctx -> BaseUrl
    manager = ctxManager ctx

    p' :: Proxy (JSONAPI (ProxiedAPI api))
    p' = Proxy

    p :: Proxy (ProxiedAPI api)
    p = Proxy

data API = Futuhours

type FutuhoursAPI = "reports" :> "missinghours" :> Get '[CSV, JSON] MissingHoursReport

instance Proxyable 'Futuhours where
    type ProxyNamespace 'Futuhours = "futuhours"
    type ProxiedAPI 'Futuhours = FutuhoursAPI

type ProxyAPI = Get '[JSON] Text
    :<|> Proxied 'Futuhours

type ProxyAPI' = FuturiceAPI ProxyAPI ('FutuAccent 'AF3 'AC3)

proxyAPI :: Proxy ProxyAPI
proxyAPI = Proxy

proxyAPI' :: Proxy ProxyAPI'
proxyAPI' = Proxy

-------------------------------------------------------------------------------
-- WAI/startup
-------------------------------------------------------------------------------

server :: Ctx -> Server ProxyAPI
server ctx = pure "P-R-O-X-Y" :<|> makeProxy (Proxy :: Proxy 'Futuhours) ctx

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
