module Futurice.App.Theme.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig

data Config = Config
    { cfgPort :: !Int
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }


instance GetConfig Config where
    getConfig = Config
        <$> parseDefaultPort "THEMAPP"
