{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports (defaultMain) where

import Futurice.Prelude

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Maybe                 (mapMaybe)
import Futurice.Servant
import Network.HTTP.Client
       (Manager, httpLbs, newManager, parseUrl, responseBody)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Network.Wai
import Servant
import System.IO                  (hPutStrLn, stderr)

import Futurice.Reflection.TypeLits (reifyTypeableSymbol)

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified GitHub                   as GH
import qualified Network.Wai.Handler.Warp as Warp

import Futurice.App.Reports.Config
import Futurice.App.Reports.Logic
import Futurice.App.Reports.Server.API
import Futurice.App.Reports.Types

-- | TODO: use reader monad
type Ctx = (DynMapCache, Manager, Config)

serveIssues :: Ctx -> ExceptT ServantErr IO IssueReport
serveIssues (cache, mgr, cfg) =
    lift $ reifyTypeableSymbol p $ cachedIO cache 600 () $ do
        repos' <- repos mgr (cfgReposUrl cfg)
        issueReport mgr (cfgGhAuth cfg) repos'
  where
    p = Proxy :: Proxy "GitHub issues"

serveFumGitHubReport :: Ctx -> ExceptT ServantErr IO FumGitHubReport
serveFumGitHubReport (cache, mgr, cfg) =
    lift $ reifyTypeableSymbol p $ cachedIO cache 600 () $
        fumGithubReport mgr cfg
  where
    p = Proxy :: Proxy "Users in FUM <-> GitHub"

-- | API server
server :: Ctx -> Server ReportsAPI
server ctx = pure IndexPage
    :<|> serveIssues ctx
    :<|> serveFumGitHubReport ctx

-- | Server with docs and cache and status
server' :: DynMapCache -> Ctx -> Server ReportsAPI'
server' cache ctx = futuriceServer
    "Report API"
    "Various reports"
    cache reportsApi (server ctx)

-- | Wai application
app :: DynMapCache -> Ctx -> Application
app cache ctx = serve reportsApi' (server' cache ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, github-dashaboard-server is alive"
    cfg@Config {..} <- getConfig
    mgr <- newManager tlsManagerSettings
    cache <- newDynMapCache
    let ctx = (cache, mgr, cfg)
    let app' = app cache ctx
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

