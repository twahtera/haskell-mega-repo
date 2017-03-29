module Futurice.App.Proxy.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Servant.Client             (BaseUrl)

import qualified FUM

data Config = Config
    { cfgPostgresConnInfo     :: !ConnectInfo
    , cfgReportsAppBaseurl    :: !BaseUrl
    , cfgPlanmillProxyBaseurl :: !BaseUrl
    , cfgGithubProxyBaseurl   :: !BaseUrl
    , cfgFumBaseurl           :: !BaseUrl
    , cfgFumAuthToken         :: !FUM.AuthToken
    , cfgPowerBaseurl         :: !BaseUrl
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
