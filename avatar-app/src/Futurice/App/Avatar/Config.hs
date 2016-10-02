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

instance HasPort Config where
    port = lens cfgPort $ \cfg p -> cfg { cfgPort = p }

getConfig :: IO Config
getConfig = Config
    <$> parseEnvVar "UNUSED"
    <*> parseDefaultPort "AVATAR"
