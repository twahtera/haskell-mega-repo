module Futurice.App.Proxy.Config (
    Config(..),
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

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

instance GetConfig Config where
    getConfig = Config
        <$> parseDefaultPort "PROXYAPP"
        <*> getConnectInfo
        <*> parseEnvVar "FUTUHOURSAPI_BASEURL"
