module Futurice.App.Checklist.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

data Config = Config
    { cfgMockAuth :: !Bool
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVarWithDefault "CHECKLIST_MOCKAUTH" False
