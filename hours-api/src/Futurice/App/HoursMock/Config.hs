module Futurice.App.HoursMock.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

data Config = Config

instance Configure Config where
    configure = pure Config
