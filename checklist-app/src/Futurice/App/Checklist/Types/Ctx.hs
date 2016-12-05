{-# LANGUAGE FlexibleContexts  #-}
module Futurice.App.Checklist.Types.Ctx (
    Ctx (..),
    newCtx,
    ctxApplyCmd,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO)
import Crypto.Random                      (newGenIO)
import Crypto.Random.DRBG                 (HmacDRBG)
import Data.Pool                          (Pool, createPool, withResource)

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Types

data Ctx = Ctx
    { ctxWorld     :: TVar World
    -- , ctxOrigWorld :: World
    , ctxPostgres  :: Pool Postgres.Connection
    , ctxPRNGs     :: Pool (TVar HmacDRBG)
    }

newCtx :: Postgres.ConnectInfo -> World -> IO Ctx
newCtx ci w = Ctx
    <$> newTVarIO w
    -- <*> pure w
    <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
    <*> createPool (newGenIO >>= newTVarIO) (\_ -> return()) 1 3600 5

ctxApplyCmd
    :: (MonadLog m, MonadBaseControl IO m, MonadIO m)
    => FUM.UserName -> Command Identity -> Ctx -> m ()
ctxApplyCmd fumuser cmd ctx = do
    liftIO $ atomically $ modifyTVar' (ctxWorld ctx) (applyCommand cmd)
    withResource (ctxPostgres ctx) $ \conn -> do
        transactCommand conn fumuser cmd
