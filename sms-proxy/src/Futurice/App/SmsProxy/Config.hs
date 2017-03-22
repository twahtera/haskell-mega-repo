module Futurice.App.SmsProxy.Config where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

data Config = Config
    { cfgTwilioUrl :: !String
      -- ^ Twilio API endpoint
    , cfgTwilioUser :: !Text
      -- ^ API SID
    , cfgTwilioPass :: !Text
      -- ^ API Secret
    , cfgTwilioSender :: !Text
      -- ^ SMS Sender ID (or phonenumber)
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "TWILIO_URL"
        <*> envVar "TWILIO_USER"
        <*> envVar "TWILIO_PASS"
        <*> envVar "TWILIO_SENDER"
