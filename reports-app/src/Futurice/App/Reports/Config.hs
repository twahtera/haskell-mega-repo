module Futurice.App.Reports.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig

import qualified GitHub             as GH
import qualified FUM

data Config = Config
    { cfgGhAuth      :: !GH.Auth
    , cfgGhOrg       :: !(GH.Name GH.Organization)
    , cfgGhTeam      :: !Text
    , cfgFumPubUrl   :: !Text
    , cfgFumAuth     :: !FUM.AuthToken     -- ^ FUM auth token
    , cfgFumBaseUrl  :: !FUM.BaseUrl       -- ^ FUM base url
    , cfgFumUserList :: !FUM.ListName      -- ^ FUM user list
    , cfgReposUrl    :: !Text
    , cfgPort        :: !Int
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
        <*> parseDefaultPort "REPORTSAPP"
