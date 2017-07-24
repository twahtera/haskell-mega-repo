{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE ApplicativeDo        #-}
#endif
module Futurice.App.HoursApi.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryEditEndpoint,
    entryDeleteEndpoint,
    ) where

import Control.Concurrent.STM    (readTVarIO)
import Control.Lens
       (Getter, filtered, firstOf, foldOf, maximumOf, sumOf, to, withIndex,
       (<&>))
import Data.Fixed                (Centi)
import Data.List                 (nubBy)
import Data.Set.Lens             (setOf)
import Data.Time                 (addDays)
import Futurice.Cache            (DynMapCache, cached)
import Futurice.CryptoRandom     (CryptoGenError)
import Futurice.Monoid           (Average (..))
import Futurice.Prelude
import Futurice.Time
       (NDT (..), TimeUnit (..), ndtConvert, ndtConvert', ndtDivide)
import Futurice.Trans.PureT
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()
import Servant                   (Handler, ServantErr (..), err400, err403)

import Control.Concurrent.Async.Lifted (Concurrently (..))

import Futurice.App.HoursApi.Ctx
import Futurice.App.HoursApi.Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FUM
import qualified PlanMill as PM

-- Note: we don't import .Monad!
import qualified Futurice.App.HoursApi.Class as H

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- | @GET /projects@
projectEndpoint :: H.MonadHours m => m [Project ReportableTask]
projectEndpoint = reportableProjects

-- | @GET /hours@
hoursEndpoint :: H.MonadHours m => Maybe Day -> Maybe Day -> m HoursResponse
hoursEndpoint ma mb = do
    today <- currentDay
    let a = fromMaybe today ma
        b = fromMaybe today mb
        interval = a ... b
    hoursResponse interval

-- | @GET /user@
userEndpoint :: H.MonadHours m => m User
userEndpoint = userResponse

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

hoursResponse :: forall m. H.MonadHours m => Interval Day -> m HoursResponse
hoursResponse interval = do
    reports <- H.timereports interval

    let entries = reportToEntry <$>  toList reports

    let markedTaskIds = Map.fromListWith (<>) $
            entries <&> \e -> (e ^. entryProjectId, Set.singleton (e ^. entryTaskId))
    marked <- toList <$> itraverse markedProject markedTaskIds

    -- reportable projects; the ones we can report
    reportable <- reportableProjects

    -- holiday names
    holidayNames <- mkHolidayNames <$> H.capacities interval

    -- working hours
    wh <- H.workingHours

    pure HoursResponse
        { _hoursResponseDefaultWorkHours   = wh
        , _hoursResponseReportableProjects = reportable
        , _hoursResponseMarkedProjects     = marked
        , _hoursResponseMonths             = mkHoursMonth interval holidayNames entries
        }
  where
    mkHolidayNames :: Foldable f => f H.Capacity -> Map Day DayType
    mkHolidayNames = toMapOf (folded . to mk . folded . ifolded)
      where
        mk :: H.Capacity -> Maybe (Day, DayType)
        mk c
            | Just desc <- c ^. H.capacityDescription
                                            = mk' (DayTypeHoliday desc)
            | (c ^. H.capacityAmount) <= 0  = mk' DayTypeZero
            | otherwise                     = Nothing
          where
            mk' x = Just (c ^. H.capacityDay, x)

    reportToEntry :: H.Timereport -> Entry
    reportToEntry tr = Entry
        { _entryId          = tr ^. H.timereportId
        , _entryProjectId   = tr ^. H.timereportProjectId
        , _entryTaskId      = tr ^. H.timereportTaskId
        , _entryDay         = tr ^. H.timereportDay
        , _entryDescription = tr ^. H.timereportComment
        , _entryClosed      = False -- TODO
        , _entryHours       = tr ^. H.timereportAmount
        , _entryBillable    = tr ^. H.timereportType
        }

    markedProject :: PM.ProjectId -> Set PM.TaskId -> m (Project MarkedTask)
    markedProject pid tids = do
        now <- currentTime
        p <- H.project pid
        tasks <- for (toList tids) $ \tid -> do
            t <- H.task tid
            pure MarkedTask
                { _mtaskId      = tid
                , _mtaskName    = t ^. H.taskName
                , _mtaskClosed  = now > t ^. H.taskFinish
                , _mtaskAbsence = p ^. H.projectAbsence
                }
        pure Project
            { _projectId     = pid
            , _projectName   = p ^. H.projectName
            , _projectTasks  = tasks
            , _projectClosed = p ^. H.projectClosed
            }

reportableProjects :: H.MonadHours m => m [Project ReportableTask]
reportableProjects = do
    now <- currentTime
    today <- currentDay

    -- Ask Planmill for reportable assignments
    reportable <- filter (\ra -> now < ra ^. H.raFinish) <$> H.reportableAssignments

    -- Tasks per project
    let tasksPerProject :: Map PM.ProjectId (NonEmpty PM.TaskId)
        tasksPerProject = Map.fromListWith (<>) $
            reportable ^.. folded . to reportableAcc

    projects <- for (Map.toList tasksPerProject) $ uncurry $ \pid tids -> do
        project <- H.project pid
        tasks <- for (toList tids) $ \tid -> do
            t <- H.task tid
            le <- H.latestEntry tid
            pure ReportableTask
                { _rtaskId             = t ^. H.taskId
                , _rtaskName           = t ^. H.taskName
                , _rtaskClosed         = False
                , _rtaskLatestEntry    = le
                , _rtaskHoursRemaining = Nothing
                }
        pure Project
            { _projectId     = pid
            , _projectName   = project ^. H.projectName
            , _projectTasks  = sortBy compareTasks tasks
            , _projectClosed = False
            }

    pure $ sortBy compareProjects projects
  where
    reportableAcc :: H.ReportableAssignment -> (PM.ProjectId, NonEmpty PM.TaskId)
    reportableAcc ra = (ra ^. H.raProjectId, pure $ ra ^. H.raTaskId)

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

userResponse :: forall m. H.MonadHours m => m User
userResponse = User
    <$> H.profileFirstName
    <*> H.profileLastName
    <*> H.flexBalance
    <*> H.vacationRemaining
    <*> utzResponse
    <*> H.profilePictureUrl
  where
    utzResponse :: m Float
    utzResponse = pure 110 -- TODO

-------------------------------------------------------------------------------
-- Old Endpoints
-------------------------------------------------------------------------------


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
    let user = Concurrently undefined
    let hr = Concurrently undefined
    runConcurrently $ EntryUpdateResponse <$> user <*> hr

------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

cachedPlanmillAction
    :: (Typeable a, PM.MonadPlanMill m, PM.MonadPlanMillC m a, MonadBaseControl IO m)
    => DynMapCache
    -> PM.PlanMill a
    -> m a
cachedPlanmillAction cache pm = cached cache 300 {- 5 minutes -} pm $
    PM.planmillAction pm

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
