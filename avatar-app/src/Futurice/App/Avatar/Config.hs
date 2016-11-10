module Futurice.App.Avatar.Config where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

-- | TODO: split config into two parts
data Config = Config
    { cfgPort    :: !Int
    , cfgEkgPort :: !Int
    }
    deriving (Show)

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort

    getConfig = Config
        <$> parseDefaultPort "AVATAR"
        <*> parseDefaultEkgPort "AVATAR"
