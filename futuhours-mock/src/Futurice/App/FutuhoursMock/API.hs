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
    :<|> "user" :> Get '[JSON] (User) -- TODO: should return logged-in User information
    :<|> "hours" :> Get '[JSON] (HoursResponse)
    :<|> "entry" :> Post '[JSON] ([Int])
    :<|> "entry" :> Capture "id" Int :> Put '[JSON] ([Int])
    :<|> "entry" :> Capture "id" Int :> Delete '[JSON] ([Int])

futuhoursApi :: Proxy FutuhoursAPI
futuhoursApi = Proxy