{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Data.Maybe                 (mapMaybe)
import Futurice.Integrations
       (IntegrationsConfig (..), beginningOfPrevMonth, runIntegrations)
import Futurice.Periocron
import Futurice.Servant
import Generics.SOP               (hcmap, hcollapse)
import GHC.TypeLits               (KnownSymbol)
import Network.HTTP.Client
       (Manager, httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Numeric.Interval.NonEmpty  ((...))
import Servant

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified GitHub               as GH

import Futurice.App.Reports.API
import Futurice.App.Reports.Balances     (BalanceReport, balanceReport)
import Futurice.App.Reports.Config
import Futurice.App.Reports.Logic
import Futurice.App.Reports.Markup
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, missingHoursReport)
import Futurice.App.Reports.Types

-- /TODO/ Make proper type
type Ctx = (DynMapCache, Manager, Config)

newtype ReportEndpoint r = ReportEndpoint (Ctx -> IO (RReport r))

class (KnownSymbol (RPath r), NFData (RReport r)) => RClass r
instance (KnownSymbol path, NFData report) => RClass (R path report)

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

serveIssues :: Ctx -> IO IssueReport
serveIssues (cache, mgr, cfg) = cachedIO cache 600 () $ do
    repos' <- repos mgr (cfgReposUrl cfg)
    issueReport mgr (cfgGhAuth cfg) repos'

serveFumGitHubReport :: Ctx -> IO FumGitHubReport
serveFumGitHubReport (cache, mgr, cfg) = cachedIO cache 600 () $
    fumGithubReport mgr cfg

serveMissingHoursReport :: Ctx -> IO MissingHoursReport
serveMissingHoursReport ctx@(cache, _, _) = cachedIO cache 600 () $ do
    now <- currentTime
    day <- currentDay
    -- TODO: end date to the last friday
    let interval = beginningOfPrevMonth day ... pred day
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        (missingHoursReport interval)

serveBalancesReport :: Ctx -> IO BalanceReport
serveBalancesReport ctx@(cache, _, _) = cachedIO cache 600 () $ do
    now <- currentTime
    day <- currentDay
    let interval = beginningOfPrevMonth day ... day
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        (balanceReport interval)

-- All report endpoints
-- this is used for api 'server' and pericron
reports :: NP ReportEndpoint Reports
reports =
    ReportEndpoint serveIssues :*
    ReportEndpoint serveFumGitHubReport :*
    ReportEndpoint serveMissingHoursReport :*
    ReportEndpoint serveBalancesReport :*
    Nil

makeServer :: Ctx -> NP ReportEndpoint reports -> Server (FoldReportsAPI reports)
makeServer _   Nil = pure indexPage
makeServer ctx (ReportEndpoint r :* rs) = liftIO (r ctx) :<|> makeServer ctx rs

-- | API server
server :: Ctx -> Server ReportsAPI
server ctx = makeServer ctx reports

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName           .~ "Report API"
    & serverDescription    .~ "Various reports"
    & serverColour         .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC3))
    & serverApp reportsApi .~ server
  where
    makeCtx :: Config -> DynMapCache -> IO Ctx
    makeCtx cfg cache = do
        manager <- newManager tlsManagerSettings
        let ctx = (cache, manager, cfg)

        _ <- spawnPeriocron (Options runStderrLoggingT 300) $ hcollapse $
            hcmap (Proxy :: Proxy RClass) (K . mkReportPeriocron ctx) reports

        return ctx

    mkReportPeriocron :: RClass r => Ctx -> ReportEndpoint r -> (Job, Intervals)
    mkReportPeriocron ctx (ReportEndpoint r) =
          (Job "Updating report " $ r ctx, tail $ every $ 60 * 60)

ctxToIntegrationsConfig :: UTCTime -> Ctx -> IntegrationsConfig
ctxToIntegrationsConfig now (_cache, mgr, cfg) = MkIntegrationsConfig
    { integrCfgManager                  = mgr
    , integrCfgNow                      = now
    -- Planmill
    , integrCfgPlanmillProxyBaseRequest = cfgPlanmillProxyBaseRequest cfg
    -- FUM
    , integrCfgFumAuthToken             = cfgFumAuth cfg
    , integrCfgFumBaseUrl               = cfgFumBaseUrl cfg
    , integrCfgFumEmployeeListName      = cfgFumUserList cfg
    }

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
