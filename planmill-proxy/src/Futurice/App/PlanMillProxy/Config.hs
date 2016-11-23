module Futurice.App.PlanMillProxy.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import PlanMill                   (Cfg (..))

import Futurice.EnvConfig

data Config = Config
    { cfgCfg              :: !Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgPort             :: !Int
    , cfgEkgPort          :: !Int
    }
    deriving (Show)

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort

    getConfig = config
        <$> parseEnvVar "PLANMILL_ADMIN"
        <*> parseEnvVar "PLANMILL_SIGNATURE"
        <*> parseEnvVar "PLANMILL_BASEURL"
        <*> getConnectInfo
        <*> parseDefaultPort "PLANMILLPROXY"
        <*> parseDefaultEkgPort "PLANMILLPROXY"
      where
        config userid apikey baseurl =
          Config (Cfg userid apikey baseurl)
