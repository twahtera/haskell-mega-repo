{-# LANGUAGE RecordWildCards     #-}
module Futurice.App.FutuHours.Tools where

import Prelude ()
import Futurice.Prelude
import Control.Exception    (bracket)
import Control.Monad.Random (evalRandIO)

import qualified Data.HashMap.Strict        as HM
import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM
import qualified PlanMill.Test              as PM (evalPlanMillIO)

import Futurice.App.FutuHours.Config          (Config (..), getConfig)
import Futurice.App.FutuHours.DecayCache      (populateData,
                                               timereportsCacheable, updateData)
import Futurice.App.FutuHours.PlanMillUserIds (planMillUserIds)

populateTimeReports :: IO ()
populateTimeReports = withStderrLogger $ \logger -> do
    Config{..} <- getConfig logger "FUTUHOURSAPI"
    let pmCfg = PM.Cfg
            { PM.cfgUserId  = cfgPlanmillAdminUser
            , PM.cfgApiKey  = cfgPlanmillSignature
            , PM.cfgBaseUrl = cfgPlanmillUrl
            }
    let ctx' = I cfgDevelopment :* I pmCfg :* I logger :* Nil

    bracket (Postgres.connect cfgPostgresConnInfo) Postgres.close $ \conn -> do
        planmillUserLookup <- planMillUserIds ctx' conn cfgFumToken cfgFumBaseurl cfgFumList
        users <- evalRandIO $ shuffleM $ HM.elems planmillUserLookup
        for_ users $ \pmUser -> do
            let pmUid = pmUser ^. PM.identifier
            print pmUid
            ts <- PM.evalPlanMillIO pmCfg $ PM.timereportsFor pmUid
            populateData timereportsCacheable conn pmUid ts

updateTimeReports :: IO ()
updateTimeReports = withStderrLogger $ \logger -> do
    Config{..} <- getConfig logger "FUTUHOURSAPI"
    let pmCfg = PM.Cfg
            { PM.cfgUserId  = cfgPlanmillAdminUser
            , PM.cfgApiKey  = cfgPlanmillSignature
            , PM.cfgBaseUrl = cfgPlanmillUrl
            }
    let ctx' = I cfgDevelopment :* I pmCfg :* I logger :* Nil

    bracket (Postgres.connect cfgPostgresConnInfo) Postgres.close $ \conn -> do
        planmillUserLookup <- planMillUserIds ctx' conn cfgFumToken cfgFumBaseurl cfgFumList
        users <- evalRandIO $ shuffleM $ HM.elems planmillUserLookup
        for_ (zip [(1::Int)..] users) $ \(i, pmUser) -> do
            let pmUid = pmUser ^. PM.identifier
            print (i, pmUid) -- TODO: monad-logger?
            updateData timereportsCacheable conn pmUid (fetch pmCfg)
  where
    fetch pmCfg pmUid interval = PM.evalPlanMillIO pmCfg $
        PM.timereportsFromIntervalFor rinterval pmUid
      where
        rinterval = PM.ResultInterval PM.IntervalStart (PM.intervalDayToIntervalUTC interval)