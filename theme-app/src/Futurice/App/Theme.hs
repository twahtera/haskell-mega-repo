{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Theme (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Servant
import Servant.Swagger.UI.Internal    (mkRecursiveEmbedded)

import Futurice.App.Theme.API
import Futurice.App.Theme.Config
import Futurice.App.Theme.Markup

-- | API server
server :: () -> Server ThemeAPI
server _ = pure indexPage :<|> static
  where
    -- | TODO: move to own file
    static = staticApp $ embeddedSettings $(mkRecursiveEmbedded "images")

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName         .~ "Thema app"
    & serverDescription  .~ "Futurice theme guidelines"
    & serverColour       .~ (Proxy :: Proxy 'FutuGreen)
    & serverApp themeApi .~ server
    & serverEnvPfx       .~ "THEMEAPP"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO ()
    makeCtx _ _ _ = pure ()
