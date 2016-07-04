{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Theme.Server.API where

import Futurice.Prelude
import Prelude          ()

import Futurice.Colour

import qualified Servant.HTML.Lucid            as Lucid

import Servant
import Servant.Futurice.Favicon

import Futurice.App.Theme.Types

type ThemeAPI = Get '[Lucid.HTML] IndexPage
type ThemeAPI' = ThemeAPI :<|> FutuFaviconAPI 'FutuGreen
 
themeApi :: Proxy ThemeAPI
themeApi = Proxy

themeApi' :: Proxy ThemeAPI'
themeApi' = Proxy
