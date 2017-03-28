module Futurice.App.EmailProxy.Config where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig
import Network.HTTP.Client (Request)

data Config = Config
    { cfgAwsSecretKey :: !Text
    , cfgAwsAccessKey :: !Text
    , cfgAwsSESUrl :: !Request
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "AWS_SECRET_KEY"
        <*> envVar "AWS_ACCESS_KEY"
        <*> envVar "AWS_SES_URL"
