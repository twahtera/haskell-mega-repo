module Futurice.App.ProxyMgmt.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

data Config = Config
    { cfgPostgresConnInfo :: !ConnectInfo
    , cfgPort             :: !Int
    , cfgEkgPort :: !Int
    }
    deriving (Show)

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort

    getConfig = Config
        <$> getConnectInfo
        <*> parseDefaultPort "PROXYMGMT"
        <*> parseDefaultEkgPort "PROXYMGMT"
