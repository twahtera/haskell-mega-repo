module Futurice.App.FutuhoursMock.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()
import Futurice.EnvConfig

data Config = Config
    { cfgPort             :: !Int
    }
    deriving (Show)

instance Configure Config where
    configure = Config <$> envVar "FUTUHOURSMOCK"
