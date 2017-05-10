module Futurice.App.FUM.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()

import qualified Personio

data Config = Config
    { cfgPostgresConnInfo   :: !ConnectInfo
    -- Personio
    , cfgPersonioCfg        :: Personio.Cfg
    -- Mock User!
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
