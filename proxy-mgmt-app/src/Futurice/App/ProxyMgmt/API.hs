{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt.API where

import Futurice.Prelude
import Prelude ()

import Futurice.App.ProxyMgmt.Types
import Servant
import Servant.HTML.Lucid

type ProxyMgmtAPI =
    Get '[HTML, JSON] AccessReport
    :<|> "users" :> Get '[HTML, JSON] UsersReport

proxyMgmtApi :: Proxy ProxyMgmtAPI
proxyMgmtApi = Proxy
