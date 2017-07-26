module Futurice.App.HoursApi.Ctx where

import Control.Concurrent.STM (TVar)
import Futurice.Cache         (DynMapCache)
import Futurice.Prelude
import Prelude ()

import qualified FUM
import qualified Network.HTTP.Client as HTTP
import qualified PlanMill            as PM
import qualified PlanMill.Worker     as PM

data Ctx = Ctx
    { ctxMockUser            :: !(Maybe FUM.UserName)
    , ctxFumPlanmillMap      :: !(TVar (HashMap FUM.UserName (FUM.User, PM.User)))
    , ctxCache               :: !DynMapCache
    , ctxLogger              :: !Logger
    , ctxManager             :: !Manager
    , ctxWorkers             :: !PM.Workers
    , ctxPlanMillHaxlBaseReq :: !HTTP.Request
    , ctxPlanmillCfg         :: !PM.Cfg
    }
