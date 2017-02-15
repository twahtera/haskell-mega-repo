{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.HoursApi.Ctx where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM (TVar)

import qualified FUM
import qualified PlanMill as PM

type PlanmillUserLookupMap = HashMap FUM.UserName (FUM.User, PM.User)

-- | A quite stable data, which we update once in a while when service is run.
data PlanmillData = PlanmillData
    { _planmillUserLookup :: !(PlanmillUserLookupMap)
    , _planmillProjects   :: !(HashMap PM.ProjectId (PM.Project, [PM.Task]))
    , _planmillTasks      :: !(HashMap PM.TaskId PM.Task)
    , _planmillCalendars  :: !(HashMap PM.CapacityCalendarId PM.CapacityCalendar)
    }
    -- TODO: add a field "reportable tasks per user"

data Ctx = Ctx
    { ctxPlanmillData :: !(TVar PlanmillData)
    , ctxMockUser     :: !(Maybe FUM.UserName)
    , ctxLogger       :: !Logger
    , ctxPlanmillCfg  :: !PM.Cfg
    }

makeLenses ''PlanmillData
