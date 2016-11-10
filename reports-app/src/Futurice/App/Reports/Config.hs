module Futurice.App.Reports.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig
import Network.HTTP.Client (Request, responseTimeout, responseTimeoutMicro)

import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH

data Config = Config
    { cfgGhOrg                    :: !(GH.Name GH.Organization)
    , cfgGithubProxyBaseRequest   :: !Request
    , cfgFumPubUrl                :: !Text
    , cfgFumAuth                  :: !FUM.AuthToken     -- ^ FUM auth token
    , cfgFumBaseUrl               :: !FUM.BaseUrl       -- ^ FUM base url
    , cfgFumUserList              :: !FUM.ListName      -- ^ FUM user list
    , cfgFlowdockAuthToken        :: !FD.AuthToken
    , cfgFlowdockOrgName          :: !(FD.ParamName FD.Organisation)
    , cfgReposUrl                 :: !Text
    , cfgPlanmillProxyBaseRequest :: !Request
    , cfgPort                     :: !Int
    , cfgEkgPort                  :: !Int
    }
    deriving (Show)

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort

    getConfig = Config
        <$> parseEnvVar "GH_ORG"
        <*> (f <$> parseEnvVar "GITHUBPROXY_HAXLURL")
        <*> parseEnvVar "FUM_PUBLICURL"
        <*> parseEnvVar "FUM_TOKEN"
        <*> parseEnvVar "FUM_BASEURL"
        <*> parseEnvVar "FUM_LISTNAME"
        <*> parseEnvVar "FD_AUTH_TOKEN"
        <*> parseEnvVar "FD_ORGANISATION"
        <*> parseEnvVar "REPORTS_GH_REPOSURL" -- TODO: change to REPORTSAPP_GH_REPOSURL
        <*> (f <$> parseEnvVar "PLANMILLPROXY_HAXLURL")
        <*> parseDefaultPort "REPORTSAPP"
        <*> parseDefaultEkgPort "REPORTSAPP"
      where
        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }
