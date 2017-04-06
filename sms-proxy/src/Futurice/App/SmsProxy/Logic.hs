{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.SmsProxy.Logic (sendSms) where

import Prelude ()
import Futurice.Prelude

import Futurice.App.SmsProxy.Types
import Futurice.App.SmsProxy.Config
import Futurice.App.SmsProxy.Ctx

import qualified Network.HTTP.Client as H
import Network.HTTP.Types.Status as S

import qualified Data.Text as T

sendSms :: (MonadIO m, MonadLog m) => Ctx -> Req -> m Res
sendSms ctx req = do
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

    pure $ Res
        { _resTo     = number
        , _resStatus = decodeUtf8Lenient $ S.statusMessage $ H.responseStatus res
        }
