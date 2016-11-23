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
    }
    deriving (Show)

instance Configure Config where
    configure = config
        <$> envVar "PLANMILL_ADMIN"
        <*> envVar "PLANMILL_SIGNATURE"
        <*> envVar "PLANMILL_BASEURL"
        <*> envConnectInfo
      where
        config userid apikey baseurl =
          Config (Cfg userid apikey baseurl)
