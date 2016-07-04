{-# LANGUAGE RecordWildCards #-}
module Futurice.App.Theme (defaultMain) where

import Futurice.Prelude

import Network.Wai              (Application)
import Servant
import Servant.Futurice.Favicon (serveFutuFavicon)
import System.IO                (hPutStrLn, stderr)

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
app = serve themeApi' (server :<|> serveFutuFavicon)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, github-dashaboard-server is alive"
    Config {..} <- getConfig
    hPutStrLn stderr $ "Starting web server in port " ++ show cfgPort
    Warp.run cfgPort app
