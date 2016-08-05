module Futurice.App.PlanMillProxy.Types (
    Ctx,
    ) where

import Futurice.Servant (DynMapCache)
import PlanMill         (Cfg)

type Ctx = (DynMapCache, Cfg)
