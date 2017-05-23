{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.HoursApi.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryEditEndpoint,
    entryDeleteEndpoint,
    ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Lens
       (Getter, filtered, firstOf, foldOf, maximumOf, sumOf, to, withIndex,
       (<&>))
import Data.Fixed             (Centi)
import Data.List              (nubBy)
import Data.Set.Lens          (setOf)
import Data.Time              (addDays)
import Futurice.Cache         (DynMapCache, cached)
import Futurice.CryptoRandom  (CryptoGenError)
import Futurice.Monoid        (Average (..))
import Futurice.Prelude
import Futurice.Time
       (NDT (..), TimeUnit (..), ndtConvert, ndtConvert', ndtDivide)
import Futurice.Trans.PureT
import Prelude ()
import Servant                (Handler, ServantErr (..), err400, err403)

import Control.Concurrent.Async.Lifted (Concurrently (..))

import Futurice.App.HoursApi.Ctx
import Futurice.App.HoursApi.Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FUM
import qualified PlanMill as PM

-- import Futurice.Generics

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- | @GET /projects@
projectEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler [Project ReportableTask]
projectEndpoint ctx mfum = do
    now <- currentTime
    authorisedUser ctx mfum $ reportableProjects (ctxCache ctx) now

-- | @GET /user@
userEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler User
userEndpoint ctx mfum = do
    now <- currentTime
    -- liftIO $ threadDelay 1000000
    authorisedUser ctx mfum $ userResponse (ctxCache ctx) now

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> Handler HoursResponse
hoursEndpoint ctx mfum start end = do
    now <- currentTime
    interval <- maybe (throwError err400 { errBody = "Interval parameters are required" }) pure interval'
    authorisedUser ctx mfum $ \_fumUser pmUser pmData ->
        hoursResponse (ctxCache ctx) now interval pmUser pmData
  where
    interval'    = (PM....) <$> start <*> end

-- | Create new entry: @POST /entry@
entryEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryEndpoint ctx mfum eu = do
    now <- currentTime
    authorisedUser ctx mfum $ \fumUser pmUser pmData -> do
        let task' = pmData ^? planmillTasks . ix (eu ^. euTaskId)
        task <- maybe (logAttention_ "cannot find task" >> lift (throwError err400 { errBody = "Unknown task" })) pure task'

        {-
        when (PM.taskProject task /= Just (eu ^. euProjectId)) $ do
            logAttention "Task and project ids don't match" (PM.taskProject task, eu ^. euProjectId, eu)
            throwError err400 { errBody = "ProjectId and TaskId don't agree" }
        -}

        let newTimeReport = PM.NewTimereport
              { PM.ntrTask    = eu ^. euTaskId
              , PM.ntrStart   = eu ^. euDate
              , PM.ntrAmount  = fmap truncate $ ndtConvert $ eu ^. euHours
              , PM.ntrComment = fromMaybe "" $ eu ^. euDescription
              , PM.ntrUser    = pmUser ^. PM.identifier
              }

        _ <- PM.planmillAction $ PM.addTimereport newTimeReport

        -- TODO: remove
        logTrace_ (textShow task)
        logTrace_ (textShow newTimeReport)

        -- Building the response
        entryUpdateResponse (ctxCache ctx) now fumUser pmUser pmData (eu ^. euDate)

-- | @PUT /entry/#id@
entryEditEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryEditEndpoint ctx mfum eid eu = do
    now <- currentTime
    authorisedUser ctx mfum $ \fumUser pmUser pmData -> do
        let task' = pmData ^? planmillTasks . ix (eu ^. euTaskId)
        _ <- maybe (logAttention_ "cannot find task" >> lift (throwError err400 { errBody = "Unknown task" })) pure task'

        timereport <- PM.planmillAction $ PM.timereport eid

        -- If new taskId is different from old one:
        -- remove marking, and create new one
        -- https://github.com/futurice/hours-ui/issues/70
        if PM.trTask timereport == eu ^. euTaskId
            then doEdit pmUser
            else do
                _ <- PM.planmillAction $ PM.deleteTimereport eid
                doCreate pmUser

        -- Building the response
        entryUpdateResponse (ctxCache ctx) now fumUser pmUser pmData (eu ^. euDate)
  where
    doEdit pmUser = do
        logTrace_ "endryEditEndpoint: editTimereport"

        let editTimereport = PM.EditTimereport
              { PM._etrId     = eid
              , PM.etrTask    = eu ^. euTaskId
              , PM.etrStart   = eu ^. euDate
              , PM.etrAmount  = fmap truncate $ ndtConvert $ eu ^. euHours
              , PM.etrComment = fromMaybe "" $ eu ^. euDescription
              , PM.etrUser    = pmUser ^. PM.identifier
              }

        void $ PM.planmillAction $ PM.editTimereport editTimereport

    doCreate pmUser = do
        logTrace_ "endryEditEndpoint: delete >> create"

        let newTimeReport = PM.NewTimereport
              { PM.ntrTask    = eu ^. euTaskId
              , PM.ntrStart   = eu ^. euDate
              , PM.ntrAmount  = fmap truncate $ ndtConvert $ eu ^. euHours
              , PM.ntrComment = fromMaybe "" $ eu ^. euDescription
              , PM.ntrUser    = pmUser ^. PM.identifier
              }

        void $ PM.planmillAction $ PM.addTimereport newTimeReport

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> Handler EntryUpdateResponse
entryDeleteEndpoint ctx mfum eid = do
    now <- currentTime
    authorisedUser ctx mfum $ \fumUser pmUser pmData -> do
        tr <- PM.planmillAction $ PM.timereport eid
        let d = PM.trStart tr

        _ <- PM.planmillAction $ PM.deleteTimereport eid

        entryUpdateResponse (ctxCache ctx) now fumUser pmUser pmData d

-------------------------------------------------------------------------------
-- Implementation
-------------------------------------------------------------------------------

type ImplM = PureT CryptoGenError Ctx Handler

entryUpdateResponse
    :: DynMapCache
    -> UTCTime
    -> FUM.User
    -> PM.User
    -> PlanmillData
    -> Day
    -> ImplM EntryUpdateResponse
entryUpdateResponse cache now fumUser pmUser pmData d = do
    let interval = d PM.... d
    let user = Concurrently $ userResponse cache now fumUser pmUser pmData
    let hr = Concurrently $ hoursResponse cache now interval pmUser pmData
    runConcurrently $ EntryUpdateResponse <$> user <*> hr

reportableProjects
    :: DynMapCache
    -> UTCTime
    -> FUM.User
    -> PM.User
    -> PlanmillData
    -> ImplM [Project ReportableTask]
reportableProjects cache now _fumUser pmUser pmData = do
    -- Ask Planmill for reportable assignments
    reps' <- toList <$> cachedPlanmillAction cache (PM.reportableAssignments pmUid)
    let reps = filter (\ra -> PM.raTaskFinish ra > now) reps'

    -- TODO: use raLastTimereportCreated?

    -- entries to guess latest entries
    let interval = addDays (-30) today PM.... addDays 7 today
    let resultInterval = PM.ResultInterval PM.IntervalStart interval
    reports <- PM.planmillAction $ PM.timereportsFromIntervalFor resultInterval pmUid

    let latestEntries :: Map PM.TaskId LatestEntry
        latestEntries = Map.fromListWith pickLatest (map toLatestEntry (toList reports))

    -- first we group tasks by project id
    let perProject :: Map PM.ProjectId (NonEmpty PM.Task)
        perProject = Map.fromListWith (<>) $ reps ^.. folded . to perProjectEl . folded

    let projects = perProject ^.. ifolded . withIndex . to (uncurry (mk' latestEntries)) . folded
    pure $ sortBy compareProjects projects
  where
    pmUid = pmUser ^. PM.identifier
    today = utctDay now

    perProjectEl
        :: PM.ReportableAssignment
        -> Maybe (PM.ProjectId, NonEmpty PM.Task)
    perProjectEl ra = (PM.raProject ra,) <$> pmData ^?  planmillTasks . ix (PM.raTask ra) . to (:| [])

    mk' :: Map PM.TaskId LatestEntry
        -> PM.ProjectId -> NonEmpty PM.Task
        -> Maybe (Project ReportableTask)
    mk' latestEntries pid ts = pmData ^? planmillProjects . ix pid . _1 <&> \p ->
         mk latestEntries p ts

    mk :: Map PM.TaskId LatestEntry
       -> PM.Project -> NonEmpty PM.Task -> Project ReportableTask
    mk latestEntries p ts = Project
        { _projectId     = p ^. PM.identifier
        , _projectName   = PM.pName p
        , _projectTasks  = nubBy ((==) `on` view rtaskId) tasks
        , _projectClosed = False  -- reportable projects aren't closed
        }
      where
        -- TODO: sort by latestEntry?
        tasks = sortBy compareTasks $ ts ^.. folded . to toTask

        toTask :: PM.Task -> ReportableTask
        toTask t = mkTask tid (PM.taskName t)
            & rtaskLatestEntry .~ (latestEntries ^? ix tid)
            & rtaskClosed      .~ (now > PM.taskFinish t)
          where
            tid = t ^. PM.identifier

    toLatestEntry :: PM.Timereport -> (PM.TaskId, LatestEntry)
    toLatestEntry tr = (,) (PM.trTask tr) $ LatestEntry
        { _latestEntryDescription = fromMaybe "" $ PM.trComment tr
        , _latestEntryDate        = PM.trStart tr
        , _latestEntryHours       = ndtConvert' $ PM.trAmount tr
        }

    pickLatest :: LatestEntry -> LatestEntry -> LatestEntry
    pickLatest a b
        | a ^. latestEntryDate > b ^. latestEntryDate = a
        | otherwise                                   = b


    -- compare latest entries dates
    compareProjects :: Project ReportableTask -> Project ReportableTask -> Ordering
    compareProjects a b = compareMaybes (maximumOf l a) (maximumOf l b)
      where
        l = projectTasks . folded . rtaskLatestEntry . folded . latestEntryDate

    compareTasks :: ReportableTask -> ReportableTask -> Ordering
    compareTasks a b = compareMaybes (maximumOf l a) (maximumOf l b)
      where
        l = rtaskLatestEntry . folded . latestEntryDate

    -- 'Just' values are smaller. Note: reversed
    compareMaybes :: Ord a => Maybe a -> Maybe a -> Ordering
    compareMaybes (Just a) (Just b) = compare b a
    compareMaybes (Just _) Nothing  = LT
    compareMaybes Nothing  (Just _) = GT
    compareMaybes Nothing  Nothing  = EQ

--  Note: we might want to not ask the cache for user vacations.
userResponse
    :: DynMapCache
    -> UTCTime
    -> FUM.User
    -> PM.User
    -> PlanmillData
    -> ImplM User
userResponse cache now fumUser pmUser pmData = runConcurrently $ mkUser
    <$> balanceResponse
    <*> holidaysLeftResponse
    <*> utzResponse
  where
    mkUser balance holidaysLeft utz = User
        { _userFirstName       = PM.uFirstName pmUser
        , _userLastName        = PM.uLastName pmUser
        , _userBalance         = balance
        , _userHolidaysLeft    = holidaysLeft
        , _userUtilizationRate = utz
        , _userProfilePicture  = fromMaybe "" $ fumUser ^. FUM.userImageUrl . lazy
        }

    balanceResponse :: Concurrently ImplM (NDT 'Hours Centi)
    balanceResponse = Concurrently $
        ndtConvert' . view PM.tbMinutes <$>
            PM.planmillAction (PM.userTimeBalance pmUid)

    holidaysLeftResponse :: Concurrently ImplM (NDT 'Days Centi)
    holidaysLeftResponse = Concurrently $ do
        let wh = workingHours (pmData ^. planmillCalendars) pmUser
        holidaysLeft <- sumOf (folded . to PM.vacationDaysRemaining . to ndtConvert') <$>
            cachedPlanmillAction cache (PM.userVacations pmUid)
        -- I wish we could do units properly.
        pure $ NDT $ ndtDivide holidaysLeft wh

    utzResponse :: Concurrently ImplM Float
    utzResponse = Concurrently $ do
        let end = localDay $ utcToHelsinkiTime now
        let start = addDays (-30) end
        let interval = start PM.... end
        let resultInterval = PM.ResultInterval PM.IntervalStart interval
        reports <- PM.planmillAction $ PM.timereportsFromIntervalFor resultInterval pmUid
        let Average _hours utz = foldOf (folded . reportUtilizationAvg) reports
        pure utz

    pmUid = pmUser ^. PM.identifier

    reportUtilizationAvg :: Getter PM.Timereport (Average Float)
    reportUtilizationAvg = to $ \tr ->
        let NDT hours = ndtConvert' (PM.trAmount tr) :: NDT 'Hours Float
        in case billableStatus (PM.trProject tr) (PM.trBillableStatus tr) of
            EntryTypeBillable    -> Average hours 100
            EntryTypeNotBillable -> Average hours 0
            EntryTypeOther       -> mempty

hoursResponse
    :: DynMapCache
    -> UTCTime
    -> PM.Interval Day
    -> PM.User
    -> PlanmillData
    -> ImplM HoursResponse
hoursResponse cache now interval pmUser pmData = do
    let resultInterval = PM.ResultInterval PM.IntervalStart interval
    let pmUid = pmUser ^. PM.identifier

    -- entries
    reports <- PM.planmillAction $ PM.timereportsFromIntervalFor resultInterval pmUid

    -- Absences have taskId but doesn't have projectId. Let's correct this
    let missingTaskIds = toList $ setOf
            (folded . to (\tr -> maybe (Just $ PM.trTask tr) (const Nothing) (PM.trProject tr)) . folded)
            reports

    missingTasks <- traverse (cachedPlanmillAction cache . PM.task) missingTaskIds

    -- taskId -> projectId, for missing sutff
    let reverseLookup = Map.fromList $ missingTasks <&> \t ->
            (t ^. PM.identifier, PM.taskProject t)

    -- patch reports, add missing projectIds.
    -- they can still be Nothing, but it's not so luckily.
    let reports' = reports <&> \tr -> tr
          { PM.trProject = PM.trProject tr <|> (reverseLookup ^? ix (PM.trTask tr) . folded)
          }

    let entries = reportToEntry <$> toList reports'

    -- marked projects; the ones in entries
    let markedTaskIds = Map.fromListWith (<>) $
            entries <&> \e -> (e ^. entryProjectId, Set.singleton (e ^. entryTaskId))

    markedProjects <- toList <$> itraverse markedProject markedTaskIds

    -- reportable projects; the ones we can report
    projects <- reportableProjects cache now undefined pmUser pmData

    -- logTrace "latestEntries" latestEntries
    -- logTrace "projects" $ (\p -> (p ^. projectName, p ^.. projectTasks . folded . taskLatestEntry . folded)) <$> projects

    -- holiday names
    capacities <- PM.planmillAction $ PM.userCapacity interval pmUid
    let holidayNames = mkHolidayNames capacities

    -- working hours
    let wh = workingHours (pmData ^. planmillCalendars) pmUser

    -- all together
    pure $ HoursResponse
        { _hoursResponseDefaultWorkHours   = wh
        , _hoursResponseReportableProjects = projects
        , _hoursResponseMarkedProjects     = markedProjects
        , _hoursResponseMonths             = mkHoursMonth interval holidayNames entries
        }
  where
    -- TODO: hardcoded value
    isAbsence :: PM.Project -> Bool
    isAbsence p = PM.pCategory p == Just 900

    markedProject :: PM.ProjectId -> Set (PM.TaskId) -> ImplM (Project MarkedTask)
    markedProject pid tids = do
        (pname, closed, absence) <- case pmData ^? planmillProjects . ix pid . _1 of
            Just p  -> pure (PM.pName p, False, False)
            Nothing -> do
                p <- cachedPlanmillAction cache (PM.project pid)
                -- TODO: absence names
                pure (PM.pName p, True, isAbsence p)

        tasks <- for (toList tids) $ \tid ->
            case pmData ^? planmillTasks . ix tid of
                Just t -> pure $ MarkedTask
                    { _mtaskId      = tid
                    , _mtaskName    = PM.taskName t
                    , _mtaskClosed  = now > PM.taskFinish t
                    , _mtaskAbsence = False
                    }
                Nothing -> pure $ MarkedTask
                    { _mtaskId = tid
                    , _mtaskName = "-"
                    , _mtaskClosed = True
                    , _mtaskAbsence = absence
                    }

        pure $ Project
            { _projectId     = pid
            , _projectName   = pname
            , _projectTasks  = tasks
            , _projectClosed = closed
            }

    mkHolidayNames :: Foldable f => f PM.UserCapacity -> Map Day DayType
    mkHolidayNames = toMapOf (folded . to mk . folded . ifolded)
      where
        mk :: PM.UserCapacity -> Maybe (Day, DayType)
        mk uc
            | Just desc <- PM.userCapacityDescription uc
                                              = if desc == ""
                                                    then mk' DayTypeZero
                                                    else mk' (DayTypeHoliday desc)
            | PM.userCapacityAmount uc == 0   = mk' DayTypeZero
            | otherwise                       = Nothing
          where
            mk' x = Just (PM.userCapacityDate uc, x)

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

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- TODO: we hard code the non-billable enumeration value.
-- TODO: absences should be EntryTypeOther, seems that Nothing projectId is the thing there.
billableStatus :: Maybe PM.ProjectId -> Int -> EntryType
billableStatus Nothing 3 = EntryTypeOther
billableStatus _ 3       = EntryTypeNotBillable
billableStatus _ _       = EntryTypeBillable

cachedPlanmillAction
    :: (Typeable a, PM.MonadPlanMill m, PM.MonadPlanMillC m a, MonadBaseControl IO m)
    => DynMapCache
    -> PM.PlanMill a
    -> m a
cachedPlanmillAction cache pm = cached cache 300 {- 5 minutes -} pm $
    PM.planmillAction pm

-- | Actually we'd like to return 'WithUnit (Hours/Day) Centi'
--
-- * /TODO/ cache calendars in @ctx@.
--
-- * /TODO/ no idea how it works for people with e.g. 30 hours a week calendars
--
workingHours
    :: HashMap PM.CapacityCalendarId PM.CapacityCalendar
    -> PM.User
    -> NDT 'Hours Centi
workingHours calendars pmUser = maybe 7.5 ndtConvert' $ do
    cid <- PM.uCalendars pmUser
    firstOf (folded . filtered (\c -> cid == c ^. PM.identifier) . to PM.ccDefaultDailyWorktime . folded) calendars

authorisedUser
    :: Ctx -> Maybe FUM.UserName
    -> (FUM.User -> PM.User -> PlanmillData -> ImplM a)
    -> Handler a
authorisedUser ctx mfum f = do
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        pmData <- liftIO $ readTVarIO $ ctxPlanmillData ctx
        (fumUser, pmUser) <- maybe (throwError err403) pure $
            pmData ^. planmillUserLookup . at fumUsername
        f fumUser pmUser pmData
            & flip runPureT ctx
