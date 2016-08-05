{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Spice.Server.API where

import Futurice.App.Spice.Types
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

type SpiceAPI = Get '[PlainText] Text
    :<|> "stats.json" :> Get '[JSON] Stats

type SpiceAPI' = FuturiceAPI SpiceAPI ('FutuAccent 'AF5 'AC2)

spiceStatsApi :: Proxy SpiceAPI
spiceStatsApi = Proxy

spiceStatsApi' :: Proxy SpiceAPI'
spiceStatsApi' = Proxy
