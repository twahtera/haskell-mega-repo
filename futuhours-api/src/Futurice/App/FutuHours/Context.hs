{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuHours.Context where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM     (TVar)
import Data.Dependent.Map         (DMap)
import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.AVar              (AVar)
import Futurice.Has               (Has (..))
import PlanMill                   (Cfg (..))

import Futurice.App.FutuHours.Types

data Ctx = Ctx
    { ctxDevelopment        :: !Development
    , ctxPlanmillCfg        :: !Cfg
    , ctxPostgresPool       :: !(Pool Connection)
    , ctxPlanmillUserLookup :: !(TVar PlanmillUserLookupTable)
    , ctxPrecalcEndpoints   :: !(DMap EndpointTag AVar)
    , ctxLogger             :: !Logger
    }

type HasDevelopment r = Has r Development
type HasPlanmillCfg r = Has r Cfg
type HasLogger      r = Has r Logger

development :: HasDevelopment r => Lens' r Development
development = field

planmillCfg :: HasPlanmillCfg r => Lens' r Cfg
planmillCfg = field

logger :: HasLogger r => Lens' r Logger
logger = field

instance Has Ctx Development where
    field = lens ctxDevelopment $ \c x -> c { ctxDevelopment = x }
instance Has Ctx Cfg where
    field = lens ctxPlanmillCfg $ \c x -> c { ctxPlanmillCfg = x }
instance Has Ctx Logger where
    field = lens ctxLogger $ \c x -> c { ctxLogger = x }
