{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.API where

import Futurice.Prelude
import Prelude ()

import Futurice.App.FutuhoursMock.Types
import Servant

type FutuhoursAPI = Get '[JSON] Text
    :<|> "projects" :> Get '[JSON] (Vector Project)

futuhoursApi :: Proxy FutuhoursAPI
futuhoursApi = Proxy
