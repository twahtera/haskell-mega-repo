module Futurice.App.FutuHours.Config (
    Config(..),
    getConfig,
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

import qualified FUM
import qualified PlanMill as PM

import Futurice.App.FutuHours.Types (Development (..))

-- | TODO: split config into two parts
data Config = Config
    { cfgPlanmillUrl       :: !String
      -- ^ Planmill url
    , cfgPlanmillAdminUser :: !PM.UserId
      -- ^ Admin user id
    , cfgPlanmillSignature :: !PM.ApiKey
      -- ^ Token
    , cfgPostgresConnInfo  :: !ConnectInfo
      -- ^ Postgres
    , cfgFumToken          :: !FUM.AuthToken
    , cfgFumBaseurl        :: !FUM.BaseUrl
    , cfgFumList           :: !FUM.ListName
    , cfgDevelopment       :: !Development
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "PLANMILL_BASEURL"
        <*> envVar "PLANMILL_ADMIN"
        <*> envVar "PLANMILL_SIGNATURE"
        <*> envConnectInfo
        <*> envVar "FUM_TOKEN"
        <*> envVar "FUM_BASEURL"
        <*> envVar "FUM_LISTNAME"
        <*> envVarWithDefault "DEVELOPMENT" Production
