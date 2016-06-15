module Futurice.App.Reports.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig

import qualified GitHub             as GH

data Config = Config
    { cfgGhAuth           :: !GH.Auth
      -- ^ Github auth information
    , cfgReposUrl         :: !Text
    , cfgPort             :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

getConfig :: IO Config
getConfig = Config
    <$> parseEnvVar "GH_AUTH_TOKEN"
    <*> parseEnvVar "REPORTS_GH_REPOSURL"
    <*> parseDefaultPort

