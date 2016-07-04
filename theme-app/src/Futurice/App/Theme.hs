{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Theme (defaultMain) where

import Futurice.Prelude

import Network.Wai                    (Application)
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Servant
import Servant.Futurice.Favicon       (serveFutuFavicon)
import Servant.Swagger.UI.Internal    (mkRecursiveEmbedded)
import System.IO                      (hPutStrLn, stderr)

import qualified Network.Wai.Handler.Warp as Warp


import Futurice.App.Theme.Config
import Futurice.App.Theme.Server.API
import Futurice.App.Theme.Types

-- | API server
server :: Server ThemeAPI
server = pure IndexPage

-- | Server with docs and cache and status

-- | Wai application
app :: Application
app = serve themeApi' (server :<|> static :<|> serveFutuFavicon)
  where
    -- | TODO: move to own file
    static = staticApp $ embeddedSettings $(mkRecursiveEmbedded "images")

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, github-dashaboard-server is alive"
    Config {..} <- getConfig
    hPutStrLn stderr $ "Starting web server in port " ++ show cfgPort
    Warp.run cfgPort app
