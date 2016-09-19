module Futurice.App.Proxy.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

data Config = Config
    { cfgPort             :: !Int
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgFutuhoursBaseurl :: !String
    }

getConfig :: IO Config
getConfig = Config
    <$> parseDefaultPort "PROXYAPP"
    <*> getConnectInfo
    <*> parseEnvVar "FUTUHOURSAPI_BASEURL"
