module Futurice.App.Theme.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig

data Config = Config
    { cfgPort        :: !Int
    }
    deriving (Show)

getConfig :: IO Config
getConfig = Config
    <$> parseDefaultPort "THEMAPP"
