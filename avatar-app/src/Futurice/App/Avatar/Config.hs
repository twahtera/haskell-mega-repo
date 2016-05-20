module Futurice.App.Avatar.Config where

import Prelude        ()
import Futurice.Prelude

import Futurice.EnvConfig

-- | TODO: split config into two parts
data Config = Config
    { cfgUnused :: !Text
      -- ^ Flowdock organisation
    , cfgPort   :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

getConfig :: IO Config
getConfig = Config
    <$> parseEnvVar "UNUSED"
    <*> parseDefaultPort
