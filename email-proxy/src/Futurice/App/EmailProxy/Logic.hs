{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.EmailProxy.Logic (sendEmail) where

import Prelude ()
import Futurice.Prelude

import Futurice.App.EmailProxy.Types

sendEmail :: (MonadIO m, MonadLog m) => Req -> m Res
sendEmail req = do
    logAttention "sendEmail: not implemented" req
    pure $ Res "not" "implemented"
