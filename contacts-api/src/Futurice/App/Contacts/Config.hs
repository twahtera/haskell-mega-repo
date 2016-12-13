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
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "FUM_AUTH_TOKEN"
        <*> envVar "FUM_BASE_URL"
        <*> envVar "FUM_USER_LIST"
        <*> (f <$> envVar "GITHUBPROXY_HAXLURL")
        <*> envVar "GH_ORGANISATION"
        <*> envVar "FD_AUTH_TOKEN"
        <*> envVar "FD_ORGANISATION"
        <*> (f <$> envVar "PLANMILLPROXY_HAXLURL")
      where
        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }
