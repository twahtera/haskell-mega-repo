{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillProxy.API where

import Futurice.App.PlanMillProxy.Types (Stats)
import Futurice.Prelude
import PlanMill.Types.Query             (SomeQuery, SomeResponse)
import Prelude ()
import Servant
import Servant.Binary.Tagged            (BINARYTAGGED)

type PlanMillProxyAPI =
    Get '[JSON] Text
    :<|> "planmill-haxl" :> ReqBody '[JSON] [SomeQuery] :> Post '[BINARYTAGGED] [Either Text SomeResponse]
    :<|> "stats" :> Get '[JSON] Stats

planmillProxyApi :: Proxy PlanMillProxyAPI
planmillProxyApi = Proxy
