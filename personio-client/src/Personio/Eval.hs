{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Personio.Eval where

import Control.Monad.Http
import Data.Aeson.Compat  (decode)
import Futurice.Clock
import Futurice.Prelude
import Personio.Request
import Personio.Types
import Prelude ()

import qualified Data.Text as T
import qualified Network.HTTP.Client as H

evalPersonioReq
    :: ( MonadHttp m, MonadThrow m
       , MonadLog m, MonadClock m
       , MonadReader env m, HasPersonioCfg env
       )
    => PersonioReq a
    -> m a
evalPersonioReq PersonioEmployees = do
    Cfg (BaseUrl baseUrl) (ClientId clientId) (ClientSecret clientSecret) <-
        view personioCfg

    -- Get access token
    let tokenUrl = baseUrl <> "/v1/auth?client_id=" <> clientId <> "&" <> "client_secret=" <> clientSecret
    tokenReq <- H.parseUrlThrow (tokenUrl ^. unpacked)
    logTrace_ "personio token request"
    (tokenDur, tokenRes) <- clocked $ httpLbs tokenReq { H.method = "POST" }
    logTrace "personio access token" (WrapResponse tokenRes)
    logTrace "personio access token duration" tokenDur
    Envelope (AccessToken token) <- decode (H.responseBody tokenRes)

    -- Perform request
    let url = (baseUrl <> "/v1/company/employees") ^. unpacked
    (dur, req) <- clocked $  H.parseUrlThrow url
    logTrace "personio employees request" url
    res <- httpLbs req
        { H.requestHeaders
            = ("Authorization", encodeUtf8 $ "Bearer " <> token)
            : H.requestHeaders req
        }
    logTrace "personio response" dur
    logTrace "response" (T.take 10000 $ decodeUtf8Lenient $ H.responseBody res ^. strict)
    Envelope employees <- decode (H.responseBody res)

    -- Done
    pure employees
