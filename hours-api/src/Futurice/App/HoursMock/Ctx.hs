module Futurice.App.HoursMock.Ctx where

import Control.Concurrent.STM (TVar, newTVarIO)
import Prelude ()
import Futurice.Prelude

import Futurice.App.HoursMock.World

newtype Ctx = Ctx (TVar World)

newCtx :: IO Ctx
newCtx = do
    now <- currentTime
    let world = cleanWorld now
    worldTVar <- newTVarIO world
    pure (Ctx worldTVar)
