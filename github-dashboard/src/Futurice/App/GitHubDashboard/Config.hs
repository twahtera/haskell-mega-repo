module Futurice.App.GitHubDashboard.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig
import Database.PostgreSQL.Simple     (ConnectInfo)

import qualified GitHub             as GH

data Config = Config
    { cfgGhAuth           :: !GH.Auth      -- ^ Github auth information
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgPort             :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

getConfig :: IO Config
getConfig = Config
    <$> parseEnvVar "GH_AUTH_TOKEN"
    <*> getConnectInfo
    <*> parseDefaultPort

