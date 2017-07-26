{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.HoursApi.Ctx where

import Control.Concurrent.STM (TVar)
import Futurice.Cache         (DynMapCache)
import Futurice.Prelude
import Prelude ()

import qualified FUM
import qualified Network.HTTP.Client as HTTP
import qualified PlanMill            as PM
import qualified PlanMill.Worker     as PM

-------------------------------------------------------------------------------
-- Planmill data
-------------------------------------------------------------------------------

type PlanmillUserLookupMap = HashMap FUM.UserName (FUM.User, PM.User)

-- | A quite stable data, which we update once in a while when service is run.
data PlanmillData = PlanmillData
    { _planmillUserLookup   :: !(PlanmillUserLookupMap)
    , _planmillProjects     :: !(HashMap PM.ProjectId (PM.Project, [PM.Task]))
    , _planmillTasks        :: !(HashMap PM.TaskId PM.Task)
    , _planmillCalendars    :: !(HashMap PM.CapacityCalendarId PM.CapacityCalendar)
    , _planmillTaskProjects :: !(HashMap PM.TaskId PM.Project)
    }
    -- TODO: add a field "reportable tasks per user"

-- TODO: mkPlanmillData from list of projects & tasks & calendars

mkPlanmillData
    :: PlanmillUserLookupMap
    -> [PM.Project]
    -> [PM.Task]
    -> [PM.CapacityCalendar]
    -> HashMap PM.UserId (Set PM.TaskId)  -- ^ reportable tasks per user
    -> PlanmillData
mkPlanmillData us ps ts cs _rts = undefined us ps ts cs

makeLenses ''PlanmillData

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxPlanmillData        :: !(TVar PlanmillData) -- todo remove?
    , ctxMockUser            :: !(Maybe FUM.UserName)
    , ctxCache               :: !DynMapCache
    , ctxLogger              :: !Logger
    , ctxManager             :: !Manager
    , ctxWorkers             :: !PM.Workers
    , ctxPlanMillHaxlBaseReq :: !HTTP.Request
    , ctxPlanmillCfg         :: !PM.Cfg -- remove?
    }
