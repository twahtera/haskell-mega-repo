module Futurice.App.FutuhoursApi.Ctx where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM (TVar)

import qualified FUM
import qualified PlanMill as PM

type PlanmillUserLookupMap = HashMap FUM.UserName PM.User

data Ctx = Ctx
    { ctxPlanmillUserLookup :: !(TVar PlanmillUserLookupMap)
    , ctxMockUser           :: !(Maybe FUM.UserName)
    }
