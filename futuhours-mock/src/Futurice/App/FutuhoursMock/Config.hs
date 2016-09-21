module Futurice.App.FutuhoursMock.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

data Config = Config
    { cfgPort             :: !Int
    }
    deriving (Show)

getConfig :: IO Config
getConfig = Config
    <$> parseDefaultPort "FUTUHOURSMOCK"
