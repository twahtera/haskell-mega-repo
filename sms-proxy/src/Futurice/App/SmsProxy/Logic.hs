{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.SmsProxy.Logic (sendSms) where

import Prelude ()
import Futurice.Prelude

import Futurice.App.SmsProxy.Types

sendSms :: (MonadIO m, MonadLog m) => Req -> m Res
sendSms req = do
    logAttention "sendSms: not implemented" req
    pure $ Res
        { _resTo     = req ^. reqTo
        , _resStatus = "not implemented"
        }
