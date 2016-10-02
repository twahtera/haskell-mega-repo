module Futurice.App.PlanMillProxy.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Logger       (LogLevel (..))
import Database.PostgreSQL.Simple (ConnectInfo)
import PlanMill                   (Cfg (..))

import Futurice.EnvConfig

data Config = Config
    { cfgCfg              :: !Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgLogLevel         :: !LogLevel
    , cfgPort             :: !Int
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

instance GetConfig Config where
    getConfig = config
        <$> parseEnvVar "PLANMILL_ADMIN"
        <*> parseEnvVar "PLANMILL_SIGNATURE"
        <*> parseEnvVar "PLANMILL_BASEURL"
        <*> getConnectInfo
        <*> parseEnvVarWithDefault "PLANMILLPROXY_LOGLEVEL" LevelInfo
        <*> parseDefaultPort "PLANMILLPROXY"
      where
        config userid apikey baseurl =
          Config (Cfg userid apikey baseurl)
