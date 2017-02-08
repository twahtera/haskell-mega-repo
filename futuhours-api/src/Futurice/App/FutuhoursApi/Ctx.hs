module Futurice.App.FutuhoursApi.Ctx where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM (TVar)

import qualified FUM
import qualified PlanMill as PM

type PlanmillUserLookupMap = HashMap FUM.UserName PM.User

-- | A quite stable data, which we update once in a while when service is run.
data PlanmillData = PlanmillData
    { planmillUserLookup :: !(PlanmillUserLookupMap)
    , planmillProjects   :: !(HashMap PM.ProjectId (PM.Project, [PM.Task]))
    , planmillTasks      :: !(HashMap PM.TaskId PM.Task)
    }

data Ctx = Ctx
    { ctxPlanmillData :: !(TVar PlanmillData)
    , ctxMockUser     :: !(Maybe FUM.UserName)
    , ctxLogger       :: !Logger
    , ctxPlanmillCfg  :: !PM.Cfg
    }
