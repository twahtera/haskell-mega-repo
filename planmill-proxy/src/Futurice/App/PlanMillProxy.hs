{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillProxy (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Servant

-- Contacts modules
import Futurice.App.PlanMillProxy.API
import Futurice.App.PlanMillProxy.Config   (Config (..), getConfig)
import Futurice.App.PlanMillProxy.Logic    (haxlEndpoint)
import Futurice.App.PlanMillProxy.Types (Ctx)

server :: Ctx -> Server PlanMillProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> liftIO . haxlEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Planmill Proxy"
    "Make faster queries to PlanMill"
    (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    getConfig cfgPort
    planmillProxyApi server
    (liftFuturiceMiddleware logStdoutDev)
    $ \(Config cfg _) cache -> return (cache, cfg)
