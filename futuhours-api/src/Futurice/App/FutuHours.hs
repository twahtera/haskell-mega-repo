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
import Servant
import Futurice.Servant
import System.IO                   (hPutStrLn, stderr)

import qualified Data.Dependent.Map            as DMap
import qualified Database.PostgreSQL.Simple    as Postgres
import qualified PlanMill                      as PM (Cfg (..))

-- FutuHours modules
import Futurice.App.FutuHours.API
import Futurice.App.FutuHours.Config          (Config (..))
import Futurice.App.FutuHours.Endpoints
import Futurice.App.FutuHours.PlanMillUserIds (planMillUserIds)
import Futurice.App.FutuHours.Precalc
import Futurice.App.FutuHours.Types

-- | API server
server :: Ctx -> Server FutuHoursAPI
server ctx = pure "Hello to futuhours api"
    :<|> addPlanmillApiKey ctx
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

defaultableEndpoints :: [(NominalDiffTime, SomeDefaultableEndpoint)]
defaultableEndpoints =
    [ (15 * 60, SDE powerAbsencesEndpoint)
    , (5  * 60, SDE powerUsersEndpoint)
    ]

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName        .~ "Futuhours API"
    & serverDescription .~ "Futuhours related stuff"
    & serverColour      .~ (Proxy :: Proxy 'FutuBlack)
    & serverApp futuhoursAPI .~ server
 where
    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx Config {..} logger _cache = do
        let pmCfg = PM.Cfg
                { PM.cfgUserId  = cfgPlanmillAdminUser
                , PM.cfgApiKey  = cfgPlanmillSignature
                , PM.cfgBaseUrl = cfgPlanmillUrl
                }
        -- Building the Ctx
        postgresPool <- createPool (Postgres.connect cfgPostgresConnInfo) Postgres.close 1 10 5

        -- Ad hoc context to get planmill users
        let ctx' = I cfgDevelopment :* I pmCfg :* I logger :* Nil
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
              , ctxLogger             = logger
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

        _ <- spawnPeriocron (Options logger 60) jobs

        return ctx
