{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Smileys.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant (SSOUser)
import Futurice.App.Smileys.Types
import Servant
import Servant.HTML.Lucid

type SmileysAPI =
    Get '[PlainText] Text
    :<|> "smileys" :> SSOUser :> ReqBody '[JSON] PostSmiley :> Post '[JSON] Res
    :<|> "smileys" :> SSOUser :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> Get '[JSON] SmileysReport

smileysApi :: Proxy SmileysAPI
smileysApi = Proxy
