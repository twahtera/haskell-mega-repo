module Futurice.App.Contacts.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig
import Network.HTTP.Client (Request, responseTimeout, responseTimeoutMicro)

import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH

-- | TODO: split config into two parts
data Config = Config
    { cfgFumAuth     :: !FUM.AuthToken     -- ^ FUM auth token
    , cfgFumBaseUrl  :: !FUM.BaseUrl       -- ^ FUM base url
    , cfgFumUserList :: !FUM.ListName      -- ^ FUM user list
    , cfgGhBaseReq   :: !Request           -- ^ github-proxy baseurl
    , cfgGhOrg       :: !(GH.Name GH.Organization)
      -- ^ Github organisation
    , cfgFdAuth      :: !FD.AuthToken      -- ^ Flowdock token
    , cfgFdOrg       :: !(FD.ParamName FD.Organisation)
      -- ^ Flowdock organisation
    , cfgPmBaseReq   :: !Request
    , cfgPort        :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

instance GetConfig Config where
    getConfig = Config
        <$> parseEnvVar "FUM_AUTH_TOKEN"
        <*> parseEnvVar "FUM_BASE_URL"
        <*> parseEnvVar "FUM_USER_LIST"
        <*> (f <$> parseEnvVar "GITHUBPROXY_HAXLURL")
        <*> parseEnvVar "GH_ORGANISATION"
        <*> parseEnvVar "FD_AUTH_TOKEN"
        <*> parseEnvVar "FD_ORGANISATION"
        <*> (f <$> parseEnvVar "PLANMILLPROXY_HAXLURL")
        <*> parseDefaultPort "CONTACTS"
      where
        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }
