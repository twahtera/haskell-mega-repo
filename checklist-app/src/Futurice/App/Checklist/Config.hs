module Futurice.App.Checklist.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig

data Config = Config
    { cfgMockAuth         :: !Bool
    , cfgPostgresConnInfo :: !ConnectInfo
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVarWithDefault "MOCKAUTH" False
        <*> envConnectInfo
