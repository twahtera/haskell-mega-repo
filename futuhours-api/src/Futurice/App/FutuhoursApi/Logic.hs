{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-- Temp: https://github.com/scrive/log/pull/28
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.FutuhoursApi.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM      (readTVarIO)
import Control.Lens                (to)
import Control.Monad.Trans.Control (defaultLiftWith, defaultRestoreT)
import Data.Aeson                  (FromJSON)
import Data.Constraint
import Futurice.Time               (ndtConvert')
import Servant
       (Handler, ServantErr (..), err400, err403, err500)

import Futurice.App.FutuhoursApi.Ctx
import Futurice.App.FutuhoursApi.Types

import qualified FUM
import qualified PlanMill      as PM
import qualified PlanMill.Test as PM

import Log.Monad (LogT (..))

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

projectEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler (Vector Project)
projectEndpoint = error "projectEndpoint: implement me"

-- | @GET /user@
userEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler User
userEndpoint ctx mfum =
    authorisedUser ctx mfum $ \_fumUsername pmUser _pmData -> do
        let pmUid = pmUser ^. PM.identifier
        balance <- ndtConvert' . view PM.tbMinutes <$>
            PM.planmillAction (PM.userTimeBalance pmUid)
        -- TODO: get from PM
        let holidaysLeft = 0
        let utz = 100
        -- TODO: change futurice-integrations,
        -- so we have FUM and PM bidirectional mapping with both User records from both
        -- available
        --
        -- profile picture is then trivial to get from the FUM data.
        let profilePicture = ""
        pure $ User
            { _userFirstName       = PM.uFirstName pmUser
            , _userLastName        = PM.uLastName pmUser
            , _userBalance         = balance
            , _userHolidaysLeft    = holidaysLeft
            , _userUtilizationRate = utz
            , _userProfilePicture  = profilePicture
            }

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> Handler HoursResponse
hoursEndpoint ctx mfum start end = do
    interval <- maybe (throwError err400) pure interval'
    let resultInterval = PM.ResultInterval PM.IntervalStart interval
    authorisedUser ctx mfum $ \_fumusername pmUser pmData -> do
        let pmUid = pmUser ^. PM.identifier
        reports <- PM.planmillAction $ PM.timereportsFromIntervalFor resultInterval pmUid
        capacities <- PM.planmillAction $ PM.userCapacity interval pmUid
        liftIO $ print capacities
        let projects = pmData ^.. planmillProjects . folded . to projectToProject
        let entries = reportToEntry <$> toList reports
        let holidayNames = mkHolidayNames capacities
        pure $ HoursResponse
            { _hoursResponseDefaultWorkHours = 7.5 -- TODO
            , _hoursResponseProjects         = projects
            , _hoursResponseMonths           = mkHoursMonth interval holidayNames entries
            }
  where
    interval'    = (PM....) <$> start <*> end

    mkHolidayNames :: Foldable f => f PM.UserCapacity -> Map Day Text
    mkHolidayNames = toMapOf (folded . to mk . folded . ifolded)
      where
        mk :: PM.UserCapacity -> Maybe (Day, Text)
        mk uc
            | Just desc <- PM.userCapacityDescription uc = Just (day, desc)
            | PM.userCapacityAmount uc == 0              = Just (day, "STAY HOME DAY") -- TODO: better name
            | otherwise                                  = Nothing
          where
            day = PM.userCapacityDate uc

    reportToEntry :: PM.Timereport -> Entry
    reportToEntry tr = Entry
        { _entryId          = tr ^. PM.identifier
        , _entryProjectId   = fromMaybe (PM.Ident 0) $ PM.trProject tr -- TODO: maybe case
        , _entryTaskId      = PM.trTask tr
        , _entryDay         = PM.trStart tr
        , _entryDescription = fromMaybe "" $ PM.trComment tr
        , _entryClosed      = False -- TODO
        , _entryHours       = ndtConvert' $ PM.trAmount tr
        , _entryBillable    = EntryTypeNotBillable -- TODO, check trBillableStatus
        }

    projectToProject :: (PM.Project, [PM.Task]) -> Project
    projectToProject (p, ts) = Project
        { _projectId     = p ^. PM.identifier
        , _projectName   = PM.pName p
        , _projectTasks  = taskToTask <$> ts
        , _projectClosed = False -- TODO
        }

    taskToTask :: PM.Task -> Task
    taskToTask t = mkTask (t ^. PM.identifier) (PM.taskName t)
        -- TODO: absence, closed, latestEntry, hoursRemaining

-- | @POST /entry@
entryEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryEndpoint ctx mfum eu =
    authorisedUser ctx mfum $ \fumusername _pmUser _pmData -> do
        logTrace "POST /entry" (fumusername, eu)
        throwError err500 { errBody = "Not implemented" }

-- | @PUT /entry/#id@
entryIdEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryIdEndpoint ctx mfum eid eu =
    authorisedUser ctx mfum $ \fumusername _pmUser _pmData -> do
        logTrace "PUT /entry" (fumusername, eid, eu)
        throwError err500 { errBody = "Not implemented" }

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> Handler EntryUpdateResponse
entryDeleteEndpoint ctx mfum eid =
    authorisedUser ctx mfum $ \fumusername _pmUser _pmData -> do
        logTrace "DELETE /entry" (fumusername, eid)
        throwError err500 { errBody = "Not implemented" }

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

authorisedUser
    :: Ctx -> Maybe FUM.UserName
    -> (FUM.UserName -> PM.User -> PlanmillData -> PlanmillT (LogT Handler) a)
    -> Handler a
authorisedUser ctx mfum f =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        pmData <- liftIO $ readTVarIO $ ctxPlanmillData ctx
        pmUser <- maybe (throwError err403) pure $
            pmData ^. planmillUserLookup . at fumUsername
        f fumUsername pmUser pmData
            & runPlanmillT (ctxPlanmillCfg ctx)
            & runLogT "endpoint" (ctxLogger ctx)


-------------------------------------------------------------------------------
-- PlanmillT: TODO move to planmill-client
-------------------------------------------------------------------------------

-- TODO: this abit of waste as it reinitialises crypto for each request
newtype PlanmillT m a = PlanmillT { runPlanmillT' :: ReaderT PM.Cfg m a }

runPlanmillT :: PM.Cfg -> PlanmillT m a -> m a
runPlanmillT cfg m = runReaderT (runPlanmillT' m) cfg

instance Functor m => Functor (PlanmillT m) where
    fmap f (PlanmillT x) = PlanmillT $ fmap f x
instance Applicative m => Applicative (PlanmillT m) where
    pure = PlanmillT . pure
    PlanmillT f <*> PlanmillT x = PlanmillT (f <*> x)
instance Monad m => Monad (PlanmillT m) where
    return = pure
    m >>= k = PlanmillT $ runPlanmillT' m >>= runPlanmillT' . k

instance MonadTrans PlanmillT where
    lift = PlanmillT . lift

instance MonadError e m => MonadError e (PlanmillT m) where
    throwError = lift . throwError
    catchError m h = PlanmillT $ runPlanmillT' m `catchError` (runPlanmillT' . h)

instance MonadIO m => MonadIO (PlanmillT m) where
    liftIO = lift . liftIO

instance MonadTransControl PlanmillT where
    type StT PlanmillT a = StT (ReaderT PM.Cfg) a
    liftWith = defaultLiftWith PlanmillT runPlanmillT'
    restoreT = defaultRestoreT PlanmillT

instance Monad m => PM.MonadPlanMillConstraint (PlanmillT m) where
    type MonadPlanMillC (PlanmillT m) = FromJSON
    entailMonadPlanMillCVector _ _ = Sub Dict

instance MonadIO m => PM.MonadPlanMill (PlanmillT m) where
    planmillAction planmill = PlanmillT $ ReaderT $ \cfg ->
        liftIO $ PM.evalPlanMillIO cfg planmill

instance MonadError e m => MonadError e (LogT m) where
    throwError = lift . throwError
    catchError m h = LogT $ unLogT m `catchError` (unLogT . h)
