module Futurice.App.Avatar.Config where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

-- | TODO: split config into two parts
data Config = Config
    deriving (Show)

instance Configure Config where
    configure = pure Config
