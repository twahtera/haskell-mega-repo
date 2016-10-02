module Futurice.App.ProxyMgmt.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

data Config = Config
    { cfgPostgresConnInfo :: !ConnectInfo
    , cfgPort             :: !Int
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

getConfig :: IO Config
getConfig = Config
    <$> getConnectInfo
    <*> parseDefaultPort "PROXYMGMT"
