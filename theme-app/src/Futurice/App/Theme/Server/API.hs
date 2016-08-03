{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Theme.Server.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Servant

import Futurice.App.Theme.Types

type ThemeAPI = Get '[HTML] IndexPage :<|> "images" :> Raw
type ThemeAPI' = FuturiceAPI ThemeAPI 'FutuGreen

themeApi :: Proxy ThemeAPI
themeApi = Proxy

themeApi' :: Proxy ThemeAPI'
themeApi' = Proxy
