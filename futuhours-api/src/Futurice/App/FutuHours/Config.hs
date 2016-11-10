module Futurice.App.FutuHours.Config (
    Config(..),
    getConfig,
    ) where

import Prelude ()
import Futurice.Prelude

import Control.Monad.Logger       (LogLevel (..))
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
    , cfgPort              :: !Int
    , cfgEkgPort           :: !Int
    , cfgDevelopment       :: !Development
    , cfgLogLevel          :: !LogLevel
    }
    deriving (Show)

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort

    getConfig = Config
        <$> parseEnvVar "PLANMILL_BASEURL"
        <*> parseEnvVar "PLANMILL_ADMIN"
        <*> parseEnvVar "PLANMILL_SIGNATURE"
        <*> getConnectInfo
        <*> parseEnvVar "FUM_TOKEN"
        <*> parseEnvVar "FUM_BASEURL"
        <*> parseEnvVar "FUM_LISTNAME"
        <*> parseDefaultPort "FUTUHOURSAPI"
        <*> parseDefaultEkgPort "FUTUHOURSAPI"
        <*> parseEnvVarWithDefault "DEVELOPMENT" Production
        <*> parseEnvVarWithDefault "LOGLEVEL" LevelInfo
