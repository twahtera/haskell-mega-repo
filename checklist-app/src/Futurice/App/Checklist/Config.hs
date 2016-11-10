module Futurice.App.Checklist.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

-- | TODO: split config into two parts
data Config = Config
    { cfgPort     :: !Int
    , cfgEkgPort  :: !Int
    , cfgMockAuth :: !Bool
    }
    deriving (Show)

instance GetConfig Config where
    port = cfgPort
    ekgPort = cfgEkgPort
    getConfig = Config
        <$> parseDefaultPort "CHECKLIST"
        <*> parseDefaultEkgPort "CHECKLIST"
        <*> parseEnvVarWithDefault "CHECKLIST_MOCKAUTH" False
