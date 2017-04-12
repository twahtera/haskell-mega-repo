{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
module Futurice.App.Reports (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Integrations
       (IntegrationsConfig (..), beginningOfPrev2Month, beginningOfPrevMonth,
       runIntegrations)
import Futurice.Periocron
import Futurice.Servant
import Generics.SOP              (hcmap, hcollapse)
import GHC.TypeLits              (symbolVal)
import Network.HTTP.Client       (httpLbs, parseUrlThrow, responseBody)
import Numeric.Interval.NonEmpty ((...))
import Servant

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified GitHub               as GH

import Futurice.App.Reports.API
import Futurice.App.Reports.Balances          (BalanceReport, balanceReport)
import Futurice.App.Reports.Config
import Futurice.App.Reports.FumFlowdock
       (FumFlowdockReport, fumFlowdockReport)
import Futurice.App.Reports.FumGithub         (FumGitHubReport, fumGithubReport)
import Futurice.App.Reports.FumPlanmill
       (FumPlanmillReport, fumPlanmillReport)
import Futurice.App.Reports.GithubIssues
       (GitHubRepo (..), IssueReport, issueReport)
import Futurice.App.Reports.GithubUsers
       (GithubUsersReport, githubUsersReport)
import Futurice.App.Reports.Markup
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, missingHoursReport)
import Futurice.App.Reports.PlanmillEmployees
       (PlanmillEmployeesReport, planmillEmployeesReport)
import Futurice.App.Reports.PowerAbsences
       (PowerAbsenceReport, powerAbsenceReport)
import Futurice.App.Reports.PowerUser         (PowerUserReport, powerUserReport)
import Futurice.App.Reports.TimereportsByTask
       (TimereportsByTaskReport, timereportsByTaskReport)

-- /TODO/ Make proper type
type Ctx = (DynMapCache, Manager, Logger, Config)

newtype ReportEndpoint r = ReportEndpoint (Ctx -> IO (RReport r))

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- Note: we cachedIO with () :: () as a key. It's ok as 'DynMapCache'
-- uses both @key@ and @value@ TypeRep's as key to non-typed map.

serveIssues :: Ctx -> IO IssueReport
serveIssues ctx@(_, mgr, _, cfg) = cachedIO' ctx () $ do
    repos' <- repos mgr (cfgReposUrl cfg)
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        (issueReport repos')

serveFumGitHubReport :: Ctx -> IO FumGitHubReport
serveFumGitHubReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        fumGithubReport

serveGithubUsersReport :: Ctx -> IO GithubUsersReport
serveGithubUsersReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        githubUsersReport

serveFumFlowdockReport :: Ctx -> IO FumFlowdockReport
serveFumFlowdockReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        fumFlowdockReport

serveFumPlanmillReport :: Ctx -> IO FumPlanmillReport
serveFumPlanmillReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        fumPlanmillReport

serveMissingHoursReport :: Ctx -> IO MissingHoursReport
serveMissingHoursReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    day <- currentDay
    -- TODO: end date to the last friday
    let interval = beginningOfPrev2Month day ... pred day
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        (missingHoursReport interval)

serveBalancesReport :: Ctx -> IO BalanceReport
serveBalancesReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    day <- currentDay
    let interval = beginningOfPrevMonth day ... day
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        (balanceReport interval)

servePowerUsersReport :: Ctx -> IO PowerUserReport
servePowerUsersReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        powerUserReport

servePowerAbsencesReport :: Ctx -> Maybe Month -> IO PowerAbsenceReport
servePowerAbsencesReport ctx mmonth = cachedIO' ctx mmonth $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        $ powerAbsenceReport mmonth

serveTimereportsByTaskReport :: Ctx -> IO TimereportsByTaskReport
serveTimereportsByTaskReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        timereportsByTaskReport

servePlanmillEmployeesReport :: Ctx -> IO PlanmillEmployeesReport
servePlanmillEmployeesReport ctx = cachedIO' ctx () $ do
    now <- currentTime
    runIntegrations
        (ctxToIntegrationsConfig now ctx)
        planmillEmployeesReport

cachedIO' :: (Eq k, Hashable k, Typeable k, Typeable v) => Ctx -> k -> IO v -> IO v
cachedIO' (cache, _, logger, _) = cachedIO logger cache 600

-- All report endpoints
-- this is used for api 'server' and pericron
reports :: NP ReportEndpoint Reports
reports =
    ReportEndpoint serveIssues :*
    ReportEndpoint serveFumGitHubReport :*
    ReportEndpoint serveFumFlowdockReport :*
    ReportEndpoint serveFumPlanmillReport :*
    ReportEndpoint serveGithubUsersReport :*
    ReportEndpoint serveMissingHoursReport :*
    ReportEndpoint serveBalancesReport :*
    ReportEndpoint serveTimereportsByTaskReport :*
    ReportEndpoint servePlanmillEmployeesReport :*
    Nil

makeServer :: Ctx -> NP ReportEndpoint reports -> Server (FoldReportsAPI reports)
makeServer _   Nil = pure indexPage
makeServer ctx (ReportEndpoint r :* rs) =
    let s = liftIO (r ctx)
    in s :<|> s :<|> s :<|> makeServer ctx rs

-- | API server
server :: Ctx -> Server ReportsAPI
server ctx = makeServer ctx reports
    :<|> liftIO (servePowerUsersReport ctx)
    :<|> liftIO . servePowerAbsencesReport ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName           .~ "Report API"
    & serverDescription    .~ "Various reports"
    & serverColour         .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC3))
    & serverApp reportsApi .~ server
    & serverEnvPfx         .~ "REPORTSAPP"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx cfg lgr cache = do
        manager <- newManager tlsManagerSettings
        let ctx = (cache, manager, lgr, cfg)

        let jobs = hcollapse $
                hcmap (Proxy :: Proxy RClass) (K . mkReportPeriocron ctx) reports

        return (ctx, jobs)

    mkReportPeriocron :: forall r. RClass r => Ctx -> ReportEndpoint r -> Job
    mkReportPeriocron ctx (ReportEndpoint r) = mkJob (name ^. packed) (r ctx)
        $ shifted (2 * 60) $ every $ 10 * 60
      where
        name = "Updating report " <> symbolVal (Proxy :: Proxy (RName r))

ctxToIntegrationsConfig :: UTCTime -> Ctx -> IntegrationsConfig I I I I
ctxToIntegrationsConfig now (_cache, mgr, lgr, Config {..}) = MkIntegrationsConfig
    { integrCfgManager                  = mgr
    , integrCfgNow                      = now
    , integrCfgLogger                   = lgr
    -- Public FUM
    , integrCfgFumPublicUrl             = cfgFumPubUrl
    -- Planmill
    , integrCfgPlanmillProxyBaseRequest = I cfgPlanmillProxyBaseRequest
    -- FUM
    , integrCfgFumAuthToken             = I cfgFumAuth
    , integrCfgFumBaseUrl               = I cfgFumBaseUrl
    , integrCfgFumEmployeeListName      = I cfgFumUserList
    -- GitHub
    , integrCfgGithubProxyBaseRequest   = I cfgGithubProxyBaseRequest
    , integrCfgGithubOrgName            = I cfgGhOrg
    -- Flowdock
    , integrCfgFlowdockToken            = I cfgFlowdockAuthToken
    , integrCfgFlowdockOrgName          = I cfgFlowdockOrgName
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
