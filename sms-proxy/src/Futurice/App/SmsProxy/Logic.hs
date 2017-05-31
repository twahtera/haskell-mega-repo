{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.SmsProxy.Logic (sendLegacySms, sendSms) where

import Prelude ()
import Futurice.Prelude

import Futurice.App.SmsProxy.Types
import Futurice.App.SmsProxy.Config
import Futurice.App.SmsProxy.Ctx

import qualified Network.HTTP.Client as H
import Network.HTTP.Types.Status as S

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

sendTwilioSms :: (MonadIO m, MonadLog m) => Ctx -> Req -> m (H.Response LBS.ByteString)
sendTwilioSms ctx req = do
    logInfo_ $ "Sending message to " <> req ^. reqTo

    let number = let num = req ^. reqTo
                     res = T.isPrefixOf "00" num
                 in case res of True  -> ("+" <>) . T.drop 2 $ num
                                False -> num

    let cfg = ctxConfig ctx
    let request = cfgTwilioBaseReq cfg
            & (\r -> r { H.method = "POST" })
            & H.applyBasicAuth (encodeUtf8 $ cfgTwilioUser cfg) (encodeUtf8 $ cfgTwilioPass cfg)
            & H.urlEncodedBody
                [ ("To", encodeUtf8 $ number)
                , ("From", encodeUtf8 $ cfgTwilioSender cfg)
                , ("Body", encodeUtf8 $ req ^. reqText)
                ]
    res <- liftIO $ H.httpLbs request (ctxManager ctx)
    pure $ res

-- | Legacy Kannel response
sendLegacySms :: (MonadIO m, MonadLog m) => Ctx -> Req -> m Text
sendLegacySms ctx req = do
    res <- sendTwilioSms ctx req

    pure $ decodeUtf8Lenient $ case S.statusMessage $ H.responseStatus res of
        "CREATED" -> "0: Accepted for delivery"
        _         -> S.statusMessage $ H.responseStatus res

sendSms :: (MonadIO m, MonadLog m) => Ctx -> Req -> m Res
sendSms ctx req = do
    res <- sendTwilioSms ctx req

    pure $ Res
        { _resTo     = req ^. reqTo
        , _resStatus = decodeUtf8Lenient $ S.statusMessage $ H.responseStatus res
        }
