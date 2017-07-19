{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Smileys.API where

import Futurice.Prelude
import Prelude ()

import Futurice.App.Smileys.Types
import Futurice.Servant           (SSOUser)
import Servant
import Servant.Chart              (Chart, SVG)

import qualified FUM

type SmileysAPI =
    Get '[PlainText] Text
    :<|> SSOUser :> "own" :> ReqBody '[JSON] PostSmiley :> Post '[JSON] Res
    :<|> SSOUser :> "own" :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> Get '[JSON] [Smileys]
    :<|> "query" :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> QueryParam "user" FUM.UserName :> Get '[JSON] [Smileys]
    :<|> "charts" :> "absolute.svg" :> Get '[SVG] (Chart "absolute")
    :<|> "charts" :> "relative.svg" :> Get '[SVG] (Chart "relative")

smileysApi :: Proxy SmileysAPI
smileysApi = Proxy
