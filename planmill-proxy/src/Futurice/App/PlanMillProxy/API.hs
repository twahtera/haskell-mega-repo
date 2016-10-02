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

import PlanMill.Types.Query  (SomeQuery)
import Servant
import Servant.Binary.Tagged (BINARYTAGGED)
import Futurice.App.PlanMillProxy.Types (SomeBinaryTagged)

type PlanMillProxyAPI =
    Get '[JSON] Text
    :<|> "haxl" :> ReqBody '[JSON] [SomeQuery] :> Post '[BINARYTAGGED] [Either Text SomeBinaryTagged]

planmillProxyApi :: Proxy PlanMillProxyAPI
planmillProxyApi = Proxy
