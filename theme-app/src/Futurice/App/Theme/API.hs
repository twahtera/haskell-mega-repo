{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Theme.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Futurice.Lucid.Foundation
import Servant

type ThemeAPI = Get '[HTML] (HtmlPage "index") :<|> "images" :> Raw

themeApi :: Proxy ThemeAPI
themeApi = Proxy
