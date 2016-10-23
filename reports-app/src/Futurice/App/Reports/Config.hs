module Futurice.App.Reports.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude

import qualified FUM
import           Futurice.EnvConfig
import qualified GitHub              as GH
import           Network.HTTP.Client
                 (Request, responseTimeout, responseTimeoutMicro)

data Config = Config
    { cfgGhAuth                   :: !GH.Auth
    , cfgGhOrg                    :: !(GH.Name GH.Organization)
    , cfgGhTeam                   :: !Text
    , cfgFumPubUrl                :: !Text
    , cfgFumAuth                  :: !FUM.AuthToken     -- ^ FUM auth token
    , cfgFumBaseUrl               :: !FUM.BaseUrl       -- ^ FUM base url
    , cfgFumUserList              :: !FUM.ListName      -- ^ FUM user list
    , cfgReposUrl                 :: !Text
    , cfgPlanmillProxyBaseRequest :: !Request
    , cfgPort                     :: !Int
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

instance GetConfig Config where
    getConfig = Config
        <$> parseEnvVar "GH_AUTH_TOKEN"
        <*> parseEnvVar "GH_ORG"
        <*> parseEnvVar "GH_TEAM"
        <*> parseEnvVar "FUM_PUBLICURL"
        <*> parseEnvVar "FUM_TOKEN"
        <*> parseEnvVar "FUM_BASEURL"
        <*> parseEnvVar "FUM_LISTNAME"
        <*> parseEnvVar "REPORTS_GH_REPOSURL" -- TODO: change to REPORTSAPP_GH_REPOSURL
        <*> (f <$> parseEnvVar "PLANMILLPROXY_HAXLURL")
        <*> parseDefaultPort "REPORTSAPP"
      where
        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }
