{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Personio.Eval where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Http
import Data.Aeson.Compat  (decode)
import Personio.Request
import Personio.Types

import qualified Network.HTTP.Client as H

evalPersonioReq
    :: ( MonadHttp m, MonadThrow m
       , MonadIO m -- todo; add logging
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
    tokenRes <- httpLbs tokenReq { H.method = "POST" }
    liftIO $ print tokenRes
    Envelope (AccessToken token) <- decode (H.responseBody tokenRes)

    -- Perform request
    let url = (baseUrl <> "/v1/company/employees") ^. unpacked
    req <- H.parseUrlThrow url
    res <- httpLbs req
        { H.requestHeaders
            = ("Authorization", encodeUtf8 $ "Bearer " <> token)
            : H.requestHeaders req
        }
    Envelope employees <- decode (H.responseBody res)

    -- Done
    pure employees
