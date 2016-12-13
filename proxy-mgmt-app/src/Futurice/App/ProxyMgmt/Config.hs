module Futurice.App.ProxyMgmt.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

data Config = Config
    { cfgPostgresConnInfo :: !ConnectInfo
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envConnectInfo
