{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuHours.Context where

import Futurice.Prelude

import Control.Concurrent.STM     (TVar)
import Control.Monad.Logger       (LogLevel (..), LoggingT, filterLogger)
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
    , ctxLogLevel           :: !LogLevel
    }

runFutuhoursLoggingT
    :: (MonadIO m, HasLogLevel env)
    => env -> LoggingT m a -> m a
runFutuhoursLoggingT env l =
    runStderrLoggingT $ filterLogger p l
  where
    p _ level = level >= (env ^. logLevel)

type HasDevelopment r = Has r Development
type HasPlanmillCfg r = Has r Cfg
type HasLogLevel    r = Has r LogLevel

development :: HasDevelopment r => Lens' r Development
development = field

planmillCfg :: HasPlanmillCfg r => Lens' r Cfg
planmillCfg = field

logLevel :: HasLogLevel r => Lens' r LogLevel
logLevel = field

instance Has Ctx Development where
    field = lens ctxDevelopment $ \c x -> c { ctxDevelopment = x }
instance Has Ctx Cfg where
    field = lens ctxPlanmillCfg $ \c x -> c { ctxPlanmillCfg = x }
instance Has Ctx LogLevel where
    field = lens ctxLogLevel $ \c x -> c { ctxLogLevel = x }
