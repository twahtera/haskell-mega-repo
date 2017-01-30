module Futurice.App.Checklist.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

import qualified FUM

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.UserName)
    , cfgPostgresConnInfo   :: !ConnectInfo
    -- ACL:
    , cfgFumToken           :: !FUM.AuthToken
    , cfgFumBaseurl         :: !FUM.BaseUrl
    -- ACL Groups
    , cfgFumITGroup         :: !FUM.GroupName
    , cfgFumHRGroup         :: !FUM.GroupName
    , cfgFumSupervisorGroup :: !FUM.GroupName
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envConnectInfo
        <*> envVar "FUM_TOKEN"
        <*> envVar "FUM_BASEURL"
        <*> envVar "FUM_IT_GROUP"
        <*> envVar "FUM_HR_GROUP"
        <*> envVar "FUM_SUPERVISOR_GROUP"
