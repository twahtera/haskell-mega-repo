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
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import Network.HTTP.Types.Status as S

sendSms :: (MonadIO m, MonadLog m, MonadThrow m, MonadError ServantErr m) => Ctx -> Req -> m Res
sendSms ctx req = do
    let cfg = ctxConfig ctx
    request0 <- H.parseRequest (cfgTwilioUrl cfg)
    let request = H.applyBasicAuth (TE.encodeUtf8 $ cfgTwilioUser cfg) (TE.encodeUtf8 $ cfgTwilioPass cfg) $
                    H.urlEncodedBody [("To", TE.encodeUtf8 $ _reqTo req),
                                      ("From", TE.encodeUtf8 $ cfgTwilioSender cfg),
                                      ("Body", TE.encodeUtf8 $ _reqText req)] request0
    manager <- liftIO $ newManager tlsManagerSettings
    res <- liftIO $ H.httpLbs request manager

    pure $ Res
        { _resTo     = req ^. reqTo
        , _resStatus = TE.decodeUtf8 $ S.statusMessage $ H.responseStatus res
        }
