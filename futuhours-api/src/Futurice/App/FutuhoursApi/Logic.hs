{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
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
import Control.Concurrent.STM (readTVarIO)
import Control.Lens           (to)
import Data.Aeson             (FromJSON)
import Data.Constraint
import Futurice.Time          (ndtConvert')
import Servant                (Handler, err400, err403)

import Futurice.App.FutuhoursApi.Ctx
import Futurice.App.FutuhoursApi.Types

import qualified FUM
import qualified PlanMill      as PM
import qualified PlanMill.Test as PM

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
userEndpoint ctx mfum = authorisedUser ctx mfum $ \_fumUsername pmUser _pmData -> do
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
        let projects = pmData ^.. planmillProjects . folded . to projectToProject
        let entries = reportToEntry <$> toList reports
        pure $ HoursResponse
            { _hoursResponseDefaultWorkHours = 7.5 -- TODO
            , _hoursResponseProjects         = projects
            , _hoursResponseMonths           = mkHoursMonth interval holidayNames entries
            }
  where
    interval'    = (PM....) <$> start <*> end
    holidayNames = mempty -- TODO

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
entryEndpoint = error "entryEndpoint: implement me"

-- | @PUT /entry/#id@
entryIdEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Int
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryIdEndpoint = error "entryIdEndpoint: implement me"

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Int
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryDeleteEndpoint = error "entryDeleteEndpoint: implement me"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

authorisedUser
    :: Ctx -> Maybe FUM.UserName
    -> (FUM.UserName -> PM.User -> PlanmillData -> PlanmillT Handler a)
    -> Handler a
authorisedUser ctx mfum f =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        pmData <- liftIO $ readTVarIO $ ctxPlanmillData ctx
        pmUser <- maybe (throwError err403) pure $
            pmData ^. planmillUserLookup . at fumUsername
        runPlanmillT (f fumUsername pmUser pmData) (ctxPlanmillCfg ctx)

-------------------------------------------------------------------------------
-- PlanmillT: TODO move to planmill-client
-------------------------------------------------------------------------------

-- TODO: this abit of waste as it reinitialises crypto for each request
newtype PlanmillT m a = PlanmillT { runPlanmillT :: PM.Cfg -> m a }

instance Functor m => Functor (PlanmillT m) where
    fmap f (PlanmillT x) = PlanmillT $ \cfg -> fmap f (x cfg)
instance Applicative m => Applicative (PlanmillT m) where
    pure = PlanmillT . const . pure
    PlanmillT f <*> PlanmillT x = PlanmillT $ \cfg -> f cfg <*> x cfg
instance Monad m => Monad (PlanmillT m) where
    return = pure
    m >>= k = PlanmillT $ \cfg -> do
      x <- runPlanmillT m cfg
      runPlanmillT (k x) cfg

instance Monad m => PM.MonadPlanMillConstraint (PlanmillT m) where
    type MonadPlanMillC (PlanmillT m) = FromJSON
    entailMonadPlanMillCVector _ _ = Sub Dict

instance MonadIO m => PM.MonadPlanMill (PlanmillT m) where
    planmillAction planmill = PlanmillT $ \cfg ->
        liftIO $ PM.evalPlanMillIO cfg planmill
