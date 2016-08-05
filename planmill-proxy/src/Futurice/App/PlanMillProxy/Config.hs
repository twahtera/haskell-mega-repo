module Futurice.App.PlanMillProxy.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude          ()

import PlanMill (Cfg (..))

import Futurice.EnvConfig

data Config = Config
    { cfgCfg  :: !Cfg
    , cfgPort :: !Int
    }
    deriving (Show)

getConfig :: IO Config
getConfig = config
    <$> parseEnvVar "PLANMILL_ADMIN"
    <*> parseEnvVar "PLANMILL_SIGNATURE"
    <*> parseEnvVar "PLANMILL_BASEURL"
    <*> parseDefaultPort "PLANMILLPROXY"
  where
    config userid apikey baseurl =
      Config (Cfg userid apikey baseurl)
