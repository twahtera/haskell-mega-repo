{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillProxy.API where

import Futurice.Prelude
import Prelude ()

import PlanMill.Types.Query  (SomeQuery, SomeResponse)
import Servant
import Servant.Binary.Tagged (BINARYTAGGED)

type PlanMillProxyAPI =
    Get '[JSON] Text
    :<|> "haxl" :> ReqBody '[JSON] [SomeQuery] :> Post '[BINARYTAGGED] [Either Text SomeResponse]

planmillProxyApi :: Proxy PlanMillProxyAPI
planmillProxyApi = Proxy
