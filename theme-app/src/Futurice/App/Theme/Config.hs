module Futurice.App.Theme.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

data Config = Config
    { cfgPort    :: !Int
    , cfgEkgPort :: !Int
    }
    deriving (Show)

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort

    getConfig = Config
        <$> parseDefaultPort "THEMAPP"
        <*> parseDefaultEkgPort "THEMAPP"
