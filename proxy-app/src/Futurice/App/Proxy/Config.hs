module Futurice.App.Proxy.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

import qualified FUM

data Config = Config
    { cfgPostgresConnInfo     :: !ConnectInfo
    , cfgReportsAppBaseurl    :: !String
    , cfgPlanmillProxyBaseurl :: !String
    , cfgGithubProxyBaseurl   :: !String
    , cfgFumBaseurl           :: !String
    , cfgFumAuthToken         :: !FUM.AuthToken
    , cfgPowerBaseurl         :: !String
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> envVar "REPORTSAPP_BASEURL"
        <*> envVar "PLANMILLPROXY_BASEURL"
        <*> envVar "GITHUBPROXY_BASEURL"
        <*> envVar "FUM_BASEURL"
        <*> envVar "FUM_TOKEN"
        <*> envVar "POWER_BASEURL"
