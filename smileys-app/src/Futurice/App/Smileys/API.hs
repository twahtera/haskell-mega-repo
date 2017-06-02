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

import qualified FUM

type SmileysAPI =
    Get '[PlainText] Text
    :<|> SSOUser :> "own" :> ReqBody '[JSON] PostSmiley :> Post '[JSON] Res
    :<|> SSOUser :> "own" :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> Get '[JSON] [Smileys]
    :<|> "query" :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> QueryParam "user" FUM.UserName :> Get '[JSON] [Smileys]

smileysApi :: Proxy SmileysAPI
smileysApi = Proxy
