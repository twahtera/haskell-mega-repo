module Futurice.App.Checklist.Config (
    Config(..),
    getConfig,
    ) where

import Futurice.Prelude
import Prelude          ()

import Futurice.EnvConfig

-- | TODO: split config into two parts
data Config = Config
    { cfgPort        :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    , cfgMockAuth    :: !Bool
    }
    deriving (Show)

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

getConfig :: IO Config
getConfig = Config
    <$> parseDefaultPort "CHECKLIST"
    <*> parseEnvVarWithDefault "CHECKLIST_MOCKAUTH" False
