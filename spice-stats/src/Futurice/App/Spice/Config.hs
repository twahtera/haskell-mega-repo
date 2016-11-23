module Futurice.App.Spice.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude          ()
import Futurice.EnvConfig

import qualified Chat.Flowdock.REST as FD
import qualified GitHub             as GH

data Config = Config
    { cfgGhAuth  :: !GH.Auth           -- ^ Github auth information
      -- ^ Github organisation
    , cfgFdAuth  :: !FD.AuthToken      -- ^ Flowdock token
    , cfgFdOrg   :: !(FD.ParamName FD.Organisation)
        -- ^ Flowdock organisation
    , cfgFdFlow  :: !(FD.ParamName FD.Flow)
        -- ^ Flowdock flow
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "GH_AUTH_TOKEN"
        <*> envVar "FD_AUTH_TOKEN"
        <*> envVar "FD_ORGANISATION"
        <*> envVar "FD_FLOW"
