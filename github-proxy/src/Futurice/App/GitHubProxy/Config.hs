module Futurice.App.GitHubProxy.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Logger       (LogLevel (..))
import Database.PostgreSQL.Simple (ConnectInfo)
import GitHub.Auth               (Auth (..))

import Futurice.EnvConfig

data Config = Config
    { cfgAuth             :: !Auth
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgLogLevel         :: !LogLevel
    , cfgPort             :: !Int
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

instance GetConfig Config where
    getConfig = Config
        <$> parseEnvVar "GH_AUTH_TOKEN"
        <*> getConnectInfo
        <*> parseEnvVarWithDefault "GITHUBPROXY_LOGLEVEL" LevelInfo
        <*> parseDefaultPort "GITHUBPROXY"
