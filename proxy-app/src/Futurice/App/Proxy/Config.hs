module Futurice.App.Proxy.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

data Config = Config
    { cfgPort                 :: !Int
    , cfgEkgPort              :: !Int
    , cfgPostgresConnInfo     :: !ConnectInfo
    , cfgReportsAppBaseurl    :: !String
    , cfgPlanmillProxyBaseurl :: !String
    }

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort

    getConfig = Config
        <$> parseDefaultPort "PROXYAPP"
        <*> parseDefaultEkgPort "PROXYAPP"
        <*> getConnectInfo
        <*> parseEnvVar "REPORTSAPP_BASEURL"
        <*> parseEnvVar "PLANMILLPROXY_BASEURL"
