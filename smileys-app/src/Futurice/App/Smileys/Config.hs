module Futurice.App.Smileys.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

import qualified FUM

data Config = Config
    { cfgMockUser          :: !(Maybe FUM.UserName)
    , cfgPostgresConnInfo  :: !ConnectInfo
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envConnectInfo

