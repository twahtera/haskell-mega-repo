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
import Prelude ()
import Servant

type SpiceAPI = Get '[PlainText] Text
    :<|> "stats.json" :> Get '[JSON] Stats

spiceStatsApi :: Proxy SpiceAPI
spiceStatsApi = Proxy
