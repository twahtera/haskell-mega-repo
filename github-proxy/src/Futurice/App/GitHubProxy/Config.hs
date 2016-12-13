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
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> (envVar "GH_AUTH_TOKEN" <!> envVar "GH_AUTH_TOKEN2")
        <*> envConnectInfo
