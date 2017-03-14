{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.SmsProxy(defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant
import Servant

import Futurice.App.SmsProxy.API
import Futurice.App.SmsProxy.Config
import Futurice.App.SmsProxy.Types
import Futurice.App.SmsProxy.Ctx
import Futurice.App.SmsProxy.Logic

server :: Ctx -> Server SmsProxyAPI
server ctx = pure "This is sms proxy" 
    :<|> (\a b -> nt $ sendSms' a b)
    :<|> (nt . sendSms)
  where
    nt :: forall x. LogT Handler x -> Handler x
    nt = runLogT "smsproxy" (ctxLogger ctx)

sendSms'
    :: (MonadIO m, MonadLog m, MonadError ServantErr m)
    => Maybe Text -> Maybe Text -> m Res
sendSms' to msg = do
    to' <- maybe (throwError $ err400 { errBody = "'to' is required" }) pure to
    msg' <- maybe (throwError $ err400 { errBody = "'msg' is required" }) pure msg
    sendSms (Req to' msg')

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "Avatar API"
    & serverDescription     .~ "Send sms"
    & serverColour          .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverApp smsProxyApi .~ server
    & serverEnvPfx          .~ "SMSPROXY"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx _cfg logger _cache = do
        return (Ctx logger, [])
