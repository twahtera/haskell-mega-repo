{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports (defaultMain) where

import Futurice.Prelude

import Control.Monad.Trans.Except (ExceptT (..))
import Data.Maybe                 (mapMaybe)
import Futurice.Servant
import Network.HTTP.Client
       (Manager, httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Servant

import Futurice.Reflection.TypeLits (reifyTypeableSymbol)

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified GitHub                   as GH

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

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Report API"
    "Various reports"
    (Proxy :: Proxy ('FutuAccent 'AF2 'AC3))
    getConfig cfgPort
    reportsApi server futuriceNoMiddleware
    $ \cfg cache -> do
        manager <- newManager tlsManagerSettings
        return (cache, manager, cfg)

-------------------------------------------------------------------------------
-- Temporary
-------------------------------------------------------------------------------

-- | We download the list
repos :: Manager -> Text -> IO [GitHubRepo]
repos mgr url = do
    req <- parseUrlThrow $ T.unpack url
    res <- TE.decodeUtf8 . LBS.toStrict . responseBody <$> httpLbs req mgr
    return $ mapMaybe f $ T.lines res
  where
    f line = case T.words line of
      [o, n] -> Just $ GitHubRepo (GH.mkOwnerName o) (GH.mkRepoName n)
      _      -> Nothing
