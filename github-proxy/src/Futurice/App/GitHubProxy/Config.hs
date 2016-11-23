module Futurice.App.GitHubProxy.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import GitHub.Auth                (Auth (..))

import Futurice.EnvConfig

data Config = Config
    { cfgAuth             :: !Auth
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgPort             :: !Int
    , cfgEkgPort          :: !Int
    }
    deriving (Show)

instance GetConfig Config where
    port    = cfgPort
    ekgPort = cfgEkgPort

    getConfig = Config
        <$> parseEnvVar "GH_AUTH_TOKEN"
        <*> getConnectInfo
        <*> parseDefaultPort "GITHUBPROXY"
        <*> parseDefaultEkgPort "GITHUBPROXY"
