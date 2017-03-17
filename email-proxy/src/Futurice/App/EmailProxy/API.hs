{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.EmailProxy.API where

import Futurice.Prelude
import Prelude ()
import Servant
import Futurice.App.EmailProxy.Types

type EmailProxyAPI =
    Get '[PlainText] Text
    :<|> "send" :> ReqBody '[JSON] Req :> Post '[JSON] Res

emailProxyApi :: Proxy EmailProxyAPI
emailProxyApi = Proxy
