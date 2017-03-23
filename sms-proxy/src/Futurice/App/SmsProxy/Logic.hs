{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.SmsProxy.Logic (sendSms) where

import Prelude ()
import Futurice.Prelude
import Servant

import Futurice.App.SmsProxy.Types
import Futurice.App.SmsProxy.Config
import Futurice.App.SmsProxy.Ctx

import qualified Network.HTTP.Client as H
import Network.HTTP.Types.Status as S

sendSms :: (MonadIO m, MonadLog m) => Ctx -> Req -> m Res
sendSms ctx req = do
    -- Log "metadata"
    logInfo_ $ "Sending message to " <> req ^. reqTo

    let cfg = ctxConfig ctx
    let request = cfgTwilioBaseReq cfg
            & (\r -> r { H.method = "POST" })
            & H.applyBasicAuth (encodeUtf8 $ cfgTwilioUser cfg) (encodeUtf8 $ cfgTwilioPass cfg)
            & H.urlEncodedBody
                [ ("To", encodeUtf8 $ req ^. reqTo)
                , ("From", encodeUtf8 $ cfgTwilioSender cfg)
                , ("Body", encodeUtf8 $ req ^. reqText)
                ]
    res <- liftIO $ H.httpLbs request (ctxManager ctx)

    pure $ Res
        { _resTo     = req ^. reqTo
        , _resStatus = decodeUtf8Lenient $ S.statusMessage $ H.responseStatus res
        }
