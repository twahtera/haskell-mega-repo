module Futurice.App.SmsProxy.Config where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig
import Network.HTTP.Client (Request)

data Config = Config
    { cfgTwilioBaseReq :: !Request  -- ^ Twilio API endpoint
    , cfgTwilioUser    :: !Text     -- ^ API SID
    , cfgTwilioPass    :: !Text     -- ^ API Secret
    , cfgTwilioSender  :: !Text     -- ^ SMS Sender ID (or phonenumber)
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "TWILIO_URL"
        <*> envVar "TWILIO_USER"
        <*> envVar "TWILIO_PASS"
        <*> envVar "TWILIO_SENDER"
