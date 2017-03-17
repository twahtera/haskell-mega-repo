{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.EmailProxy(defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant
import Servant

import Futurice.App.EmailProxy.API
import Futurice.App.EmailProxy.Config
import Futurice.App.EmailProxy.Ctx
import Futurice.App.EmailProxy.Logic

server :: Ctx -> Server EmailProxyAPI
server ctx = pure "This is email proxy" 
    :<|> (nt . sendEmail)
  where
    nt :: forall x. LogT Handler x -> Handler x
    nt = runLogT "emailproxy" (ctxLogger ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName              .~ "Avatar API"
    & serverDescription       .~ "Send Emails"
    & serverColour            .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverApp emailProxyApi .~ server
    & serverEnvPfx            .~ "EMAILPROXY"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx _cfg logger _cache = do
        return (Ctx logger, [])
