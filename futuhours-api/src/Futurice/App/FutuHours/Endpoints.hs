{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
-- | API endpoints
module Futurice.App.FutuHours.Endpoints (
    Ctx(..),
    addPlanmillApiKey,
    getProjects,
    -- getTimereports,
    -- * Power
    getPowerUsers,
    powerUsersEndpoint,
    getPowerAbsences,
    powerAbsencesEndpoint,
    -- * Legacy endpoins
    getLegacyUsers,
    getLegacyHours,
    ) where

import Futurice.Prelude
import Futurice.Time
import Prelude ()

import Control.Concurrent.STM           (readTVarIO)
import Control.Monad.Trans.Except       (ExceptT)
import Data.BinaryFromJSON              (BinaryFromJSON)
import Data.Fixed                       (Centi)
import Data.Maybe                       (fromJust)
import Data.Pool                        (withResource)
import Data.Time                        (addDays)
import Data.Time.Fxtra                  (beginningOfPrevMonth)
import Database.PostgreSQL.Simple.Fxtra (execute)
import Generics.SOP                     (All)
import Servant                          (ServantErr)

import Servant.Server (err400, err404)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
--import qualified Data.Text.Encoding  as TE
import qualified Data.Vector as V

import Futurice.App.FutuHours.Context
import Futurice.App.FutuHours.PlanMill
import Futurice.App.FutuHours.PlanMillCache
import Futurice.App.FutuHours.Precalc
import Futurice.App.FutuHours.Types

-- Planmill modules
import           Control.Monad.PlanMill (ForallFSymbol, MonadPlanMill (..))
import qualified PlanMill               as PM
import qualified PlanMill.Test          as PM (evalPlanMillIO)

-- | Add planmill api key.
addPlanmillApiKey :: MonadIO m => Ctx -> FUMUsername -> PlanmillApiKey -> m ()
addPlanmillApiKey Ctx { ctxPostgresPool = pool } username apikey =
    liftIO $ withResource pool $ \conn -> do
        rows <- execute conn "INSERT INTO futuhours.apikeys (fum_username, planmill_apikey) VALUES (?, ?)" (username, apikey)
        print username
        print apikey
        print rows

-- | Return projects for user
--
-- TODO: Add short living cache (15min?)
-- TODO: see <https://github.com/futurice/futuhours-api/issues/1>
getProjects :: MonadIO m => Ctx -> UserId -> m (V.Vector Project)
getProjects Ctx { ctxPlanmillCfg = cfg } (UserId uid) =
    liftIO $ nubVector . fmap pmToFh <$> PM.evalPlanMillIO cfg planmill
  where
    -- TODO: there is somewhere better one, I'm sure.
    nubVector :: Eq a => V.Vector a -> V.Vector a
    nubVector = V.fromList . nub . V.toList

    planmill :: PM.PlanMill PM.ReportableAssignments
    planmill = PM.reportableAssignments $ PM.Ident $ fromIntegral uid

    pmToFh :: PM.ReportableAssignment -> Project
    pmToFh PM.ReportableAssignment{..} = Project raProject raProjectName

getLegacyUsers
    :: (MonadIO m, MonadError ServantErr m)
    => Ctx -> Maybe Text -> m (Envelope User)
getLegacyUsers = withLegacyPlanmill p $ \uid -> do
    u <- planmillAction $ PM.user uid
    PM.TimeBalance balance <- planmillAction $ PM.userTimeBalance uid
    let user = User
            { userFirstName        = PM.uFirstName u
            , userDefaultWorkHours = 7.5      -- TODO
            , userHolidaysDaysLeft = 999      -- TODO
            , userBalance          = ndtConvert' balance
            , userEmployeeType     = "foo"    -- TODO
            }
    pure $ Envelope $ V.singleton user
  where
    p = Proxy :: Proxy '[PM.User, PM.TimeBalance]

getLegacyHours
    :: (MonadIO m, MonadError ServantErr m)
    => Maybe Day  -- ^ Day GTE, @>=@
    -> Maybe Day  -- ^ Day TTE, @<=@
    -> Ctx -> Maybe Text -> m (Envelope Hour)
getLegacyHours gteDay lteDay =
    case join (PM.mkResultInterval PM.IntervalStart <$> lte <*> gte) of
        Nothing       -> \_ _ -> throwError err400
        Just interval -> withLegacyPlanmill p $ \uid -> do
            let ri = interval
            ts <- PM.planmillVectorAction $ PM.timereportsFromIntervalFor ri uid
            pure $ Envelope $ fmap f ts
  where
    lte, gte :: Maybe UTCTime
    lte = UTCTime <$> lteDay <*> pure 0
    gte = UTCTime <$> gteDay <*> pure 0

    p = Proxy :: Proxy '[PM.Timereports]

    f :: PM.Timereport -> Hour
    f t = Hour
        { hourAbsence         = False
        , hourBillable        = PM.trBillableStatus t == 1 -- TODO: use enumerations
        , hourDay             = PM.trStart t
        , hourDescription     = fromMaybe "-" $ PM.trComment t
        , hourEditable        = False -- TODO: use status?
        , hourHours           = ndtConvert' $ PM.trAmount t
        , hourId              = t ^. PM.identifier
        , hourProjectId       = fromMaybe (PM.Ident 0) $ PM.trProject t
        , hourProjectCategory = 0 -- TODO
        , hourProjectName     = "TODO: project" -- TODO
        , hourStatus          = 0 -- TODO
        , hourTaskId          = PM.trTask t
        , hourTaskName        = "TODO: task" -- TODO
        , hourUserId          = PM.trPerson t
        , hourUser            = "someuser" -- TODO
        }

-------------------------------------------------------------------------------
-- Power
-------------------------------------------------------------------------------

powerUsersEndpoint
    :: DefaultableEndpoint '[] () (Vector PowerUser)
powerUsersEndpoint = DefaultableEndpoint
    { defEndTag = EPowerUsers
    , defEndDefaultParsedParam = pure ()
    , defEndDefaultParams = Nil
    , defEndParseParams = \Nil -> pure ()
    , defEndAction = powerUsers
    }
  where
    powerUsers :: Ctx -> () -> IO (Vector PowerUser)
    powerUsers ctx () = do
        pmUsers <- V.fromList . HM.toList <$> readTVarIO (ctxPlanmillUserLookup ctx)
        executeCachedAdminPlanmill ctx p $ traverse powerUser pmUsers
      where
        p = Proxy :: Proxy '[PM.User, PM.Team, PM.Meta]

        powerUser :: PM.MonadPlanMill n => (FUMUsername, PM.User) -> n PowerUser
        powerUser (fumLogin, u) = do
            t <- traverse (PM.planmillAction . PM.team) (PM.uTeam u)
            a <- PM.enumerationValue (PM.uPassive u) "-"
            return $ PowerUser
                { powerUserUsername = fumLogin
                , powerUserFirst    = PM.uFirstName u
                , powerUserLast     = PM.uLastName u
                , powerUserTeam     = maybe "Unknown Team" PM.tName t
                , powerUserStart    = PM.uHireDate u
                , powerUserEnd      = PM.uDepartDate u
                , powerUserActive   = a
                }

getPowerUsers :: Ctx -> ExceptT ServantErr IO (Vector PowerUser)
getPowerUsers = servantEndpoint powerUsersEndpoint

powerAbsencesEndpoint
    :: DefaultableEndpoint '[Maybe Day, Maybe Day] (PM.Interval Day) (Vector PowerAbsence)
powerAbsencesEndpoint = DefaultableEndpoint
    { defEndTag = EPowerAbsences
    , defEndDefaultParsedParam = do
        m <- currentDay
        let a = beginningOfPrevMonth m
        let b = addDays 365 m
        return $ fromJust $ PM.mkInterval a b
    , defEndDefaultParams = I Nothing :* I Nothing :* Nil
    , defEndParseParams = \(I a :* I b :* Nil) -> do
        b' <- maybe (addDays 365 <$> currentDay) pure b
        let a' = fromMaybe (addDays (-365) $ beginningOfPrevMonth b') a
        maybe (throwError err400) pure $ PM.mkInterval a' b'
    , defEndAction = powerAbsences
    }
  where
    powerAbsences
        :: Ctx -> PM.Interval Day -> IO (Vector PowerAbsence)
    powerAbsences ctx interval = do
        idsLookup <- fmap (view PM.identifier) <$> readTVarIO (ctxPlanmillUserLookup ctx)
        executeCachedAdminPlanmill ctx p (getPowerAbsences' idsLookup)
      where
        p = Proxy :: Proxy '[PM.Absences, PM.UserCapacities]

        getPowerAbsences'
            :: PM.MonadPlanMill n
            => HM.HashMap FUMUsername PM.UserId
            -> n (Vector PowerAbsence)
        getPowerAbsences' idsLookup = do
            absences <- PM.planmillVectorAction (PM.absencesFromInterval (toResultInterval interval))
            traverse (toPowerAbsence' idsLookup) absences

        toPowerAbsence'
            :: PM.MonadPlanMill n
            => HM.HashMap FUMUsername PM.UserId
            -> PM.Absence -> n PowerAbsence
        toPowerAbsence' idsLookup ab =
            case PM.mkInterval (PM.absenceStart ab) (PM.absenceFinish ab) of
                Just interval' -> do
                    uc <- PM.planmillVectorAction $ PM.userCapacity interval' (PM.absencePerson ab)
                    return $ toPowerAbsence idsLookup ab uc
                Nothing ->
                    return $ toPowerAbsence idsLookup ab mempty

        toPowerAbsence
            :: HM.HashMap FUMUsername PM.UserId
            -> PM.Absence -> PM.UserCapacities -> PowerAbsence
        toPowerAbsence idsLookup ab uc = PowerAbsence
            { powerAbsenceUsername     = reverseLookup (PM.absencePerson ab) idsLookup
            , powerAbsenceStart        = PM.absenceStart ab
            , powerAbsenceEnd          = PM.absenceFinish ab
            , powerAbsencePlanmillId   = ab ^. PM.identifier
            , powerAbsenceCapacities   = uc'
            , powerAbsenceBusinessDays = length uc'
            }
          where
            uc' = capacities uc

    capacities :: PM.UserCapacities -> Map Day (NDT 'Hours Centi)
    capacities
        = Map.fromList
        . filter ((> 0) . snd)
        . map (\x -> (PM.userCapacityDate x, ndtConvert' $ PM.userCapacityAmount x))
        . toList

getPowerAbsences
    :: Ctx -> Maybe Day -> Maybe Day -> ExceptT ServantErr IO (Vector PowerAbsence)
getPowerAbsences = servantEndpoint powerAbsencesEndpoint

toResultInterval :: PM.Interval Day -> PM.ResultInterval
toResultInterval = PM.ResultInterval PM.IntervalStart . PM.intervalDayToIntervalUTC

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withLegacyPlanmill
    :: forall m a as. (MonadIO m, MonadError ServantErr m, All BinaryFromJSON as)
    => Proxy as
    -> (forall n. MonadPlanMill n => PM.UserId -> n a)
    -> Ctx
    -> Maybe Text
    -> m a
withLegacyPlanmill p action ctx httpRemoteUser = do
    planmillLookup <- liftIO $ readTVarIO  (ctxPlanmillUserLookup ctx)
    let mPlanMillId = do
            fumUserName <- FUMUsername <$> httpRemoteUser
            view PM.identifier <$> HM.lookup fumUserName planmillLookup
    planMillId <- maybe (throwError err404) return mPlanMillId
    executeUncachedAdminPlanmill ctx p (action planMillId)

executeUncachedAdminPlanmill
    :: forall m a as. (MonadIO m, All BinaryFromJSON as)
    => Ctx
    -> Proxy as
    -> (forall n. MonadPlanMill n => n a)
    -> m a
executeUncachedAdminPlanmill ctx _ action =
    liftIO $ withResource (ctxPostgresPool ctx) $ \conn ->
        runHaxl ctx conn action

executeCachedAdminPlanmill
    :: forall m a as. (MonadIO m, All BinaryFromJSON as, ForallFSymbol BinaryFromJSON PM.EnumDesc)
    => Ctx
    -> Proxy as
    -> (forall n. (MonadPlanMill n, MonadPlanMillCached n) => n a)
    -> m a
executeCachedAdminPlanmill ctx _ action =
    liftIO $ withResource (ctxPostgresPool ctx) $ \conn ->
        runHaxl ctx conn action
