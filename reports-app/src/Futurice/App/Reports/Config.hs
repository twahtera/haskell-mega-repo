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
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "GH_ORG"
        <*> (f <$> envVar "GITHUBPROXY_HAXLURL")
        <*> envVar "FUM_PUBLICURL"
        <*> envVar "FUM_TOKEN"
        <*> envVar "FUM_BASEURL"
        <*> envVar "FUM_LISTNAME"
        <*> envVar "FD_AUTH_TOKEN"
        <*> envVar "FD_ORGANISATION"
        <*> envVar "REPORTS_GH_REPOSURL" -- TODO: change to REPORTSAPP_GH_REPOSURL
        <*> (f <$> envVar "PLANMILLPROXY_HAXLURL")
      where
        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }
