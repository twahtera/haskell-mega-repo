module Futurice.App.Proxy.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig

data Config = Config
    { cfgPort             :: !Int
    , cfgFutuhoursBaseurl :: !String
    }

-- | TODO: parse
getConfig :: IO Config
getConfig = Config
    <$> parseDefaultPort
    <*> parseEnvVar "FUTUHOURS_BASEURL"
