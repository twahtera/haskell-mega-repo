module Futurice.App.FutuhoursApi.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig
import Network.HTTP.Client (Request, responseTimeout, responseTimeoutMicro)

import qualified FUM
import qualified PlanMill as PM

data Config = Config
    { cfgPlanmillUrl       :: !String
      -- ^ Planmill url
    , cfgPlanmillAdminUser :: !PM.UserId
      -- ^ Admin user id
    , cfgPlanmillSignature :: !PM.ApiKey
      -- ^ Token
    , cfgPlanmillProxyReq  :: !Request
    , cfgFumToken          :: !FUM.AuthToken
    , cfgFumBaseurl        :: !FUM.BaseUrl
    , cfgFumList           :: !FUM.ListName
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "PLANMILL_BASEURL"
        <*> envVar "PLANMILL_ADMIN"
        <*> envVar "PLANMILL_SIGNATURE"
        <*> (f <$> envVar "PLANMILLPROXY_HAXLURL")
        <*> envVar "FUM_TOKEN"
        <*> envVar "FUM_BASEURL"
        <*> envVar "FUM_LISTNAME"
      where
        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }
