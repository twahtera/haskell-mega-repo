module Futurice.App.Spice.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig

import qualified Chat.Flowdock.REST as FD
import qualified GitHub             as GH

data Config = Config
    { cfgGhAuth :: !GH.Auth           -- ^ Github auth information
      -- ^ Github organisation
    , cfgFdAuth :: !FD.AuthToken      -- ^ Flowdock token
    , cfgFdOrg  :: !(FD.ParamName FD.Organisation)
        -- ^ Flowdock organisation
    , cfgFdFlow :: !(FD.ParamName FD.Flow)
        -- ^ Flowdock flow
    , cfgPort   :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

getConfig :: IO Config
getConfig = Config
    <$> parseEnvVar "GH_AUTH_TOKEN"
    <*> parseEnvVar "FD_AUTH_TOKEN"
    <*> parseEnvVar "FD_ORGANISATION"
    <*> parseEnvVar "FD_FLOW"
    <*> parseDefaultPort "SPICESTATSAPP"
