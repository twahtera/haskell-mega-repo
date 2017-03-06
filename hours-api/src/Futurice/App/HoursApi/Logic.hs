{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-- Temp: https://github.com/scrive/log/pull/28
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.HoursApi.Logic (
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
import Control.Lens
       (Fold, contains, filtered, firstOf, nullOf, sumOf, to, (<&>))
import Control.Monad.Trans.Control
       (ComposeSt, defaultLiftBaseWith, defaultLiftWith, defaultRestoreM,
       defaultRestoreT)
import Data.Aeson                  (FromJSON)
import Data.Constraint
import Data.Fixed                  (Centi)
import Data.Set.Lens               (setOf)
import Futurice.Cache              (cached)
import Futurice.Time               (NDT, TimeUnit (..), ndtConvert', ndtDivide)
import Log.Monad                   (LogT (..))
import Servant
       (Handler, ServantErr (..), err400, err403, err500)

import Futurice.App.HoursApi.Ctx
import Futurice.App.HoursApi.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified FUM
import qualified PlanMill            as PM
import qualified PlanMill.Test       as PM

-- import Futurice.Generics

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
    authorisedUser ctx mfum userResponse

userResponse
    :: FUM.User
    -> PM.User
    -> PlanmillData
    -> PlanmillT (LogT Handler) User
userResponse fumUser pmUser pmData = do
        let pmUid = pmUser ^. PM.identifier
        balance <- ndtConvert' . view PM.tbMinutes <$>
            PM.planmillAction (PM.userTimeBalance pmUid)
        let wh = workingHours (pmData ^. planmillCalendars) pmUser
        holidaysLeft <- sumOf (folded . to PM.vacationDaysRemaining . to ndtConvert') <$>
            PM.planmillAction (PM.userVacations pmUid)
        -- I wish we could do units properly.
        let holidaysLeft' = pure (ndtDivide holidaysLeft wh) :: NDT 'Days Centi
        -- TODO: calculate the UTZ (for this month?).
        let utz = 95
        pure $ User
            { _userFirstName       = PM.uFirstName pmUser
            , _userLastName        = PM.uLastName pmUser
            , _userBalance         = balance
            , _userHolidaysLeft    = holidaysLeft'
            , _userUtilizationRate = utz
            , _userProfilePicture  = fromMaybe "" $ fumUser ^. FUM.userImageUrl . lazy
            }

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> Handler HoursResponse
hoursEndpoint ctx mfum start end = do
    now <- currentTime
    interval <- maybe (throwError err400) pure interval'
    let resultInterval = PM.ResultInterval PM.IntervalStart interval
    authorisedUser ctx mfum $ \_fumUser pmUser pmData -> do
        let pmUid = pmUser ^. PM.identifier

        -- entries
        reports <- PM.planmillAction $ PM.timereportsFromIntervalFor resultInterval pmUid

        -- Absences have taskId but doesn't have projectId. Let's correct this
        let missingTaskIds = toList $ setOf
                (folded . to (\tr -> maybe (Just $ PM.trTask tr) (const Nothing) (PM.trProject tr)) . folded)
                reports

        missingTasks <- traverse (cachedPlanmillAction . PM.task) missingTaskIds
        missingProjects <- traverse (cachedPlanmillAction . PM.project)
            (missingTasks ^.. folded . to PM.taskProject . folded)
        let missingProjects' = groupProjectsTasks missingProjects missingTasks

        -- taskId -> projectId, for missing sutff
        let reverseLookup = Map.fromList $ missingTasks <&> \t ->
                (t ^. PM.identifier, PM.taskProject t)

        -- patch reports, add missing projectIds.
        -- they can still be Nothing, but it's not so luckily.
        let reports' = reports <&> \tr -> tr
              { PM.trProject = PM.trProject tr <|> (reverseLookup ^? ix (PM.trTask tr) . folded)
              }

        let entries = reportToEntry <$> toList reports'
        let latestEntries = Map.fromListWith takeLatestEntry $
                (\e -> (e ^. entryTaskId, latestEntryFromEntry e)) <$> entries

        -- task ids, we don't want to show all projects in the list
        reps <- cachedPlanmillAction $ PM.reportableAssignments pmUid
        let reportableTaskIds = setOf (folded . to PM.raTask) reps
        let reportedTaskIds   = setOf (folded . to PM.trTask) reports

        -- Let's take prefetched projects, and add missingProjects'
        let pmProjects = (pmData ^. planmillProjects) <> HM.fromList (missingProjects' <&> \p -> (p ^. _1 . PM.identifier, p))

        -- generate response projects
        let projects = sortBy projectLatestEntryCompare $ pmProjects ^..
              folded
              . to (projectToProject now latestEntries reportableTaskIds reportedTaskIds)
              . folded

        -- logTrace "latestEntries" latestEntries
        -- logTrace "projects" $ (\p -> (p ^. projectName, p ^.. projectTasks . folded . taskLatestEntry . folded)) <$> projects

        -- holiday names
        capacities <- PM.planmillAction $ PM.userCapacity interval pmUid
        let holidayNames = mkHolidayNames capacities

        -- working hours
        let wh = workingHours (pmData ^. planmillCalendars) pmUser

        -- all together
        pure $ HoursResponse
            { _hoursResponseDefaultWorkHours = wh
            , _hoursResponseProjects         = projects
            , _hoursResponseMonths           = mkHoursMonth interval holidayNames entries
            }
  where
    interval'    = (PM....) <$> start <*> end

    cachedPlanmillAction
        :: (Typeable a, PM.MonadPlanMill m, PM.MonadPlanMillC m a, MonadBaseControl IO m)
        => PM.PlanMill a -> m a
    cachedPlanmillAction pm = cached (ctxCache ctx) 300 {- 5 minutes -} pm $
        PM.planmillAction pm

    takeLatestEntry :: LatestEntry -> LatestEntry -> LatestEntry
    takeLatestEntry a b
        | a ^. latestEntryDate > b ^. latestEntryDate = a
        | otherwise                                   = b

    groupProjectsTasks :: [PM.Project] -> [PM.Task] -> [(PM.Project, [PM.Task])]
    groupProjectsTasks ps ts = ps <&> \p -> (p, ts' ^. ix (p ^. PM.identifier))
      where
        ts' = Map.fromListWith (++) $
            ts ^.. folded . to (\t -> PM.taskProject t <&> \i -> (i, [t])) . folded

    projectLatestEntryCompare :: Project -> Project -> Ordering
    projectLatestEntryCompare a b = case (firstOf l a, firstOf l b) of
        (Just a', Just b') -> compare b' a' -- latest (day is bigger) is smaller
        (Just _, Nothing)  -> LT
        (Nothing, Just _)  -> GT
        (Nothing, Nothing) -> compare (a ^. projectName) (b ^. projectName)
      where
        l :: Fold Project Day
        l = projectTasks . folded . taskLatestEntry . folded . latestEntryDate

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
        , _entryProjectId   = fromMaybe (PM.Ident 0) $ PM.trProject tr
        , _entryTaskId      = PM.trTask tr
        , _entryDay         = PM.trStart tr
        , _entryDescription = fromMaybe "" $ PM.trComment tr
        , _entryClosed      = False -- TODO
        , _entryHours       = ndtConvert' $ PM.trAmount tr
        , _entryBillable    = billableStatus (PM.trProject tr) (PM.trBillableStatus tr)
        }

    -- TODO: we hard code the non-billable enumeration value.
    -- TODO: absences should be EntryTypeOther, seems that Nothing projectId is the thing there.
    billableStatus :: Maybe PM.ProjectId -> Int -> EntryType
    billableStatus Nothing 3 = EntryTypeOther
    billableStatus _ 3       = EntryTypeNotBillable
    billableStatus _ _       = EntryTypeBillable

    projectToProject
        :: UTCTime                    -- ^ current timestamp
        -> Map PM.TaskId LatestEntry  -- ^ last entry per task
        -> Set PM.TaskId              -- ^ reportable
        -> Set PM.TaskId              -- ^ reported
        -> (PM.Project, [PM.Task])
        -> Maybe Project
    projectToProject now latestEntries reportable reported (p, ts)
        | null tasks = Nothing
        | otherwise  = Just $ Project
            { _projectId     = p ^. PM.identifier
            , _projectName   = PM.pName p
            , _projectTasks  = tasks
            , _projectClosed = closed
            }
      where
        -- TODO: use proper enumerations.
        isAbsence = PM.pCategory p == Just 900

        tasks = ts ^.. folded
            . filtered (\t -> taskIds ^. contains (t ^. PM.identifier))
            . to (\t -> taskToTask now isAbsence (latestEntries ^. at (t ^. PM.identifier)) t)

        -- Project is closed, if there aren't reportable tasks anymore.
        closed =  flip nullOf ts
            $ folded
            . filtered (\t -> reportable ^. contains (t ^. PM.identifier))

        taskIds = reportable <> reported

    taskToTask :: UTCTime -> Bool -> Maybe LatestEntry -> PM.Task -> Task
    taskToTask now isAbsence latestEntry t = mkTask (t ^. PM.identifier) (PM.taskName t)
        & taskLatestEntry .~ latestEntry
        & taskClosed      .~ (now > PM.taskFinish t)
        & taskAbsence     .~ isAbsence
        -- TODO: hoursRemaining

-- | Create new entry: @POST /entry@
entryEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryEndpoint ctx mfum eu =
    authorisedUser ctx mfum $ \fumUser pmUser pmData -> do
        let task' = pmData ^? planmillTasks . ix (eu ^. euTaskId)
        task <- maybe (logAttention_ "cannot find task" >> throwError err400 { errBody = "Unknown task" }) pure task'

        {-
        when (PM.taskProject task /= Just (eu ^. euProjectId)) $ do
            logAttention "Task and project ids don't match" (PM.taskProject task, eu ^. euProjectId, eu)
            throwError err400 { errBody = "ProjectId and TaskId don't agree" }
        -}

        logTrace_ (textShow task)
        -- TODO: write hour report

        -- Building the response
        -- TODO: build projects and hours map
    
        -- working hours
        let wh = workingHours (pmData ^. planmillCalendars) pmUser
        let hur = HoursResponse wh mempty mempty
        user <- userResponse fumUser pmUser pmData
        pure $ EntryUpdateResponse user hur

-- | @PUT /entry/#id@
entryIdEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryIdEndpoint ctx mfum eid eu =
    authorisedUser ctx mfum $ \fumUser _pmUser _pmData -> do
        logTrace "PUT /entry" (fumUser ^. FUM.userName, eid, eu)
        throwError err500 { errBody = "Not implemented" }

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> Handler EntryUpdateResponse
entryDeleteEndpoint ctx mfum eid =
    authorisedUser ctx mfum $ \fumUser _pmUser _pmData -> do
        logTrace "DELETE /entry" (fumUser ^. FUM.userName, eid)
        throwError err500 { errBody = "Not implemented" }

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Actually we'd like to return 'WithUnit (Hours/Day) Centi'
--
-- * /TODO/ cache calendars in @ctx@.
--
-- * /TODO/ no idea how it works for people with e.g. 30 hours a week calendars
workingHours
    :: HashMap PM.CapacityCalendarId PM.CapacityCalendar
    -> PM.User
    -> NDT 'Hours Centi
workingHours calendars pmUser = maybe 7.5 ndtConvert' $ do
    cid <- PM.uCalendars pmUser
    firstOf (folded . filtered (\c -> cid == c ^. PM.identifier) . to PM.ccDefaultDailyWorktime . folded) calendars

authorisedUser
    :: Ctx -> Maybe FUM.UserName
    -> (FUM.User -> PM.User -> PlanmillData -> PlanmillT (LogT Handler) a)
    -> Handler a
authorisedUser ctx mfum f =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        pmData <- liftIO $ readTVarIO $ ctxPlanmillData ctx
        (fumUser, pmUser) <- maybe (throwError err403) pure $
            pmData ^. planmillUserLookup . at fumUsername
        f fumUser pmUser pmData
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

instance MonadBase b m => MonadBase b (PlanmillT m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (PlanmillT m) where
    type StM (PlanmillT m) a = ComposeSt PlanmillT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

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
