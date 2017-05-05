{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.EmailProxy.Logic (sendEmail, getSignature) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.EmailProxy.Config
import Futurice.App.EmailProxy.Ctx
import Futurice.App.EmailProxy.Types

import Control.Lens (to)

import qualified Data.ByteString.Base64 as B64
import           Data.Digest.Pure.SHA
import qualified Data.List.NonEmpty     as NE
import           Data.Time              (defaultTimeLocale, formatTime)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import qualified Network.HTTP.Client       as H
import           Network.HTTP.Types        (Header)
import           Network.HTTP.Types.Status as S

addHeader :: Header -> H.Request -> H.Request
addHeader header req = req { H.requestHeaders = header : H.requestHeaders req}

getSignature :: ByteString -> ByteString -> ByteString
getSignature s p = B64.encode $ BL.toStrict $ bytestringDigest $ d
    where
      d = hmacSha256 (BL.fromStrict s) (BL.fromStrict p)

mkAwsAuthHeader :: Text -> Text -> String -> Header
mkAwsAuthHeader ak as moment =
    ("X-Amzn-Authorization", encodeUtf8 $ T.intercalate ", " . hfmt $
       [("AWS3-HTTPS AWSAccessKeyId", T.unpack $ ak)
       , ("Algorithm", "HmacSHA256")
       , ("Signature", T.unpack $ decodeUtf8Lenient signature)])
    where
      signature = getSignature (encodeUtf8 $ as) (encodeUtf8 . T.pack $ moment)
      hfmt = map (\x -> T.pack $ fst x ++ "=" ++ snd x)

sendSES :: (MonadIO m, MonadLog m) => Ctx -> Req -> m Res
sendSES ctx req = do
    let cfg = ctxConfig ctx
    now <- currentTime
    let moment = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" now
    -- TODO: support multiple recipients
    let request = cfgAwsSESUrl cfg
            & (\r -> r { H.method = "POST" })
            & addHeader ("Date", encodeUtf8 $ T.pack $ moment)
            & (addHeader $ mkAwsAuthHeader (cfgAwsAccessKey cfg) (cfgAwsSecretKey cfg) moment)
            & H.urlEncodedBody
                 [ ("Action", "SendEmail")
                 , ("Source", encodeUtf8 $ req ^. reqFrom)
                 , ("Destination.ToAddresses.member.1", encodeUtf8 $ req ^. reqTo . to NE.head)
                 , ("Message.Subject.Data", encodeUtf8 $ req ^. reqSubject)
                 , ("Message.Body.Text.Data", encodeUtf8 $ req ^. reqBody)
                 ]
    res <- liftIO $ H.httpLbs request (ctxManager ctx)
    {- TODO: parse XML response to obtain Error or SendEmailResult
    <ErrorResponse xmlns="http://ses.amazonaws.com/doc/2010-12-01/">
    <Error><Type>string</Type><Code>string</Code><Message>string</Message></Error>
    <RequestId>string</RequestId>
    </ErrorResponse>

    <SendEmailResponse xmlns="http://ses.amazonaws.com/doc/2010-12-01/">
    <SendEmailResult><MessageId>string</MessageId></SendEmailResult>
    <ResponseMetadata><RequestId>string/RequestId></ResponseMetadata>
    </SendEmailResponse>
    -}
    pure $ Res { _resMessage = "done"
               , _resStatus = decodeUtf8Lenient $ S.statusMessage $ H.responseStatus res
               }

sendEmail :: (MonadIO m, MonadLog m) => Ctx -> Req -> m Res
sendEmail ctx req = do
    logInfo_ $ "Sending Email to: " <> req ^. reqTo . to NE.head
    r <- sendSES ctx req
    pure $ r
