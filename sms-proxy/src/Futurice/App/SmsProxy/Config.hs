module Futurice.App.SmsProxy.Config where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

data Config = Config
    { -- TODO: add integration data
    }
    deriving (Show)

instance Configure Config where
    configure = pure Config
