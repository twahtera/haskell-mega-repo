{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fcontext-stack=30 #-}
module Futurice.App.FutuHours (defaultMain) where

import Futurice.Prelude

import Control.Concurrent.Async    (async)
import Control.Concurrent.STM      (atomically, newTVarIO, writeTVar)
import Data.Pool                   (createPool, withResource)
import Futurice.AVar
import Futurice.Periocron
import Generics.SOP                (I (..), NP (..))
import Network.Wai
import Network.Wai.Metrics         (metrics, registerWaiMetrics)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Servant.Cache.Class         (DynMapCache)
import Servant.Futurice
import System.IO                   (hPutStrLn, stderr)
import System.Metrics              (newStore, registerGcMetrics)
import System.Remote.Monitoring    (forkServerWith)

import qualified Data.Dependent.Map            as DMap
import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Network.Wai.Handler.Warp      as Warp
import qualified PlanMill                      as PM (Cfg (..))
import qualified Servant.Cache.Internal.DynMap as DynMap

-- FutuHours modules
import Futurice.App.FutuHours.API
import Futurice.App.FutuHours.Config          (Config (..), getConfig)
import Futurice.App.FutuHours.Endpoints
import Futurice.App.FutuHours.PlanMillUserIds (planMillUserIds)
import Futurice.App.FutuHours.Precalc
import Futurice.App.FutuHours.Types

-- | API server
server :: Ctx -> Server FutuHoursAPI
server ctx = pure "Hello to futuhours api"
    :<|> addPlanmillApiKey ctx
    :<|> getBalances ctx
    :<|> (getMissingHoursReport ctx
         )
    :<|> (getPowerUsers ctx
        :<|> getPowerAbsences ctx
        )
    -- :<|> getTimereports ctx
    :<|> getProjects ctx
    :<|> pure (Envelope empty)
    :<|> getLegacyUsers ctx
    :<|> (\un lte gte -> getLegacyHours lte gte ctx un)

-------------------------------------------------------------------------------
-- Startup
-------------------------------------------------------------------------------

-- | Server with docs and cache and status
server' :: DynMapCache -> Ctx -> Server FutuHoursAPI'
server' cache ctx = futuriceApiServer cache futuhoursAPI (server ctx)

-- | Wai application
app :: DynMapCache -> Ctx -> Application
app cache ctx = simpleCors $ serve futuhoursAPI' (server' cache ctx)

defaultableEndpoints :: [(NominalDiffTime, SomeDefaultableEndpoint)]
defaultableEndpoints =
    [ (15 * 60, SDE powerAbsencesEndpoint)
    , (5  * 60, SDE powerUsersEndpoint)
    , (21 * 60, SDE missingHoursEndpoint)
    , (31 * 60, SDE balanceReportEndpoint)
    ]

ekg :: Int -> IO Middleware
ekg port = do
    s <- newStore
    registerGcMetrics s
    wai <- registerWaiMetrics s
    -- Server is unused
    _ <- forkServerWith s "localhost" port
    pure (metrics wai)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, I'm futuhours-api server"
    Config{..} <- getConfig
    let pmCfg = PM.Cfg
            { PM.cfgUserId  = cfgPlanmillAdminUser
            , PM.cfgApiKey  = cfgPlanmillSignature
            , PM.cfgBaseUrl = cfgPlanmillUrl
            }
    -- Building the Ctx
    postgresPool <- createPool (Postgres.connect cfgPostgresConnInfo) Postgres.close 1 10 5

    -- Ad hoc context to get planmill users
    let ctx' = I cfgDevelopment :* I pmCfg :* I cfgLogLevel :* Nil
    planmillUserLookup <- withResource postgresPool $ \conn ->
        planMillUserIds ctx' conn cfgFumToken cfgFumBaseurl cfgFumList
    planmillUserLookupTVar <- newTVarIO planmillUserLookup

    -- Context without precalc endpoints
    let ctx'' = Ctx
          { ctxDevelopment        = cfgDevelopment
          , ctxPlanmillCfg        = pmCfg
          , ctxPostgresPool       = postgresPool
          , ctxPlanmillUserLookup = planmillUserLookupTVar
          , ctxPrecalcEndpoints   = DMap.empty
          , ctxLogLevel           = cfgLogLevel
          }

    precalcEndpoints <-
        let f m (_, SDE de) = do
                hPutStrLn stderr $ "Initialising " ++ show (defEndTag de)
                a <- async (defEndDefaultParsedParam de >>= defEndAction de ctx'')
                avar <- newAVarAsyncIO a
                pure $ DMap.insert (defEndTag de) avar m
        in foldM f DMap.empty defaultableEndpoints

    let ctx = ctx'' { ctxPrecalcEndpoints = precalcEndpoints }

    -- Periocron
    let pmUsersJob :: Job
        pmUsersJob = Job "Planmill users" $ do
            planmillUserLookup' <- withResource postgresPool $ \conn ->
                planMillUserIds ctx' conn cfgFumToken cfgFumBaseurl cfgFumList
            atomically $ writeTVar planmillUserLookupTVar planmillUserLookup'

    let precalcJobs :: [(Job, Intervals)]
        precalcJobs = flip map defaultableEndpoints $ \(interval, SDE de) ->
            let label = show (defEndTag de) ^. packed
                action = cronEndpoint de ctx
                intervals = tail $ every interval
            in (Job label action, intervals)

    let jobs = precalcJobs ++
           [ (pmUsersJob, tail $ every $ 7 * 60)
           ]

    _ <- spawnPeriocron (Options runStderrLoggingT' 60) jobs

    -- EKG
    metricsMiddleware <- ekg cfgEkgPort

    -- Startup
    cache <- DynMap.newIO
    let app' = metricsMiddleware $ app cache ctx
    hPutStrLn stderr $ "Now I'll start the webservice at port " ++ show cfgPort
    Warp.run cfgPort app'
  where
    -- Cannot eta-unexpand, breaks on GHC 7.8
    runStderrLoggingT'
        :: forall a. (forall m. (Applicative m, MonadLogger m, MonadIO m) => m a)
        -> IO a
    runStderrLoggingT' x = runStderrLoggingT x
