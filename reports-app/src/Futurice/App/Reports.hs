{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports (defaultMain) where

import Futurice.Prelude

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Development.GitRev         (gitHash)
import Network.HTTP.Client        (Manager, newManager, parseUrl, responseBody, httpLbs)
import Data.Maybe (mapMaybe)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Network.Wai
import Servant
import Servant.Cache.Class        (DynMapCache, cachedIO)
import System.IO                  (hPutStrLn, stderr)

import Futurice.Reflection.TypeLits (reifyTypeableSymbol)

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified GitHub as GH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

import Futurice.App.Reports.Config
import Futurice.App.Reports.Logic
import Futurice.App.Reports.Types
import Futurice.App.Reports.Server.API

-- | TODO: use reader monad
type Ctx = (DynMapCache, Manager, Config)

serveIssues :: Ctx -> ExceptT ServantErr IO IssueReport
serveIssues (cache, mgr, cfg) =
    lift $ reifyTypeableSymbol p $ cachedIO cache 600 () $ do
        repos' <- repos mgr (cfgReposUrl cfg)
        issueReport mgr (cfgGhAuth cfg) repos'
  where
    p = Proxy :: Proxy "GitHub issues"

-- | API server
server :: Ctx -> Server ReportsAPI 
server ctx = pure IndexPage
    :<|> serveIssues ctx

-- | Server with docs and cache and status
server' :: DynMapCache -> String -> Ctx -> Server ReportsAPI'
server' cache versionHash ctx = serverAvatarApi cache versionHash (server ctx)

-- | Wai application
app :: DynMapCache -> String -> Ctx -> Application
app cache versionHash ctx = serve avatarApi' (server' cache versionHash ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, github-dashaboard-server is alive"
    cfg@Config {..} <- getConfig
    mgr <- newManager tlsManagerSettings
    cache <- DynMap.newIO
    let ctx = (cache, mgr, cfg)
    let app' = app cache $(gitHash) ctx
    hPutStrLn stderr $ "Starting web server in port " ++ show cfgPort
    Warp.run cfgPort app'

-------------------------------------------------------------------------------
-- Temporary
-------------------------------------------------------------------------------

-- | We download the list
repos :: Manager -> Text -> IO [GitHubRepo]
repos mgr url = do
    req <- parseUrl $ T.unpack url
    res <- TE.decodeUtf8 . LBS.toStrict . responseBody <$> httpLbs req mgr
    return $ mapMaybe f $ T.lines res
  where
    f line = case T.words line of
      [o, n] -> Just $ GitHubRepo (GH.mkOwnerName o) (GH.mkRepoName n)
      _      -> Nothing
    
