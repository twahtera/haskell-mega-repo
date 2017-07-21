{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PlanMill.Worker where

import Control.Concurrent             (ThreadId, forkFinally)
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue
       (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar
       (TMVar, newEmptyTMVar, putTMVar, readTMVar)
import Control.Exception              (throwIO)
import Control.Monad.Http             (runHttpT)
import Data.Aeson.Compat              (FromJSON)
import Futurice.CryptoRandom          (mkCryptoGen, runCRandTThrow')
import Futurice.Prelude
import Prelude ()

import PlanMill.Eval  (evalPlanMill)
import PlanMill.Types

data Workers = Workers
    { workerChan    :: TBQueue Job
    , workerThreads :: [ThreadId]
    }

data Job where
    Job :: (FromJSON a, NFData a)
        => TMVar (Either SomeException a) -> PlanMill a -> Job

submitPlanMill
    :: (FromJSON a, NFData a)
    => Workers
    -> PlanMill a
    -> IO a
submitPlanMill ws pm = submitPlanMillE ws pm >>= either throwIO return

submitPlanMillE
    :: (FromJSON a, NFData a)
    => Workers
    -> PlanMill a
    -> IO (Either SomeException a)
submitPlanMillE (Workers q _) pm = do
    tmvar <- atomically $ do
        tmvar <- newEmptyTMVar
        writeTBQueue q (Job tmvar pm)
        return tmvar
    atomically (readTMVar tmvar)

workers
    :: Logger
    -> Manager
    -> Cfg
    -> [Text]   -- ^ names
    -> IO Workers
workers lgr mgr cfg names = do
    runLogT "workers" lgr $
        logInfo "Spawning workers" names
    q <- atomically (newTBQueue 1000) -- arbitrary size
    tids <- for names $ \name -> do
        g <- mkCryptoGen
        forkFinally (loop name q g) cleanup
    pure (Workers q tids)
  where
    cleanup :: Either SomeException () -> IO ()
    cleanup e = runLogT "workers" lgr $
        logAttention "Thread died" (show e)

    loop name q g = do
        Job tmvar pm <- atomically (readTBQueue q)

        e <- tryDeep
            $ flip runHttpT mgr
            $ runLogT name lgr
            $ flip runReaderT cfg
            $ runCRandTThrow' g
            $ evalPlanMill pm

        case e of
            Left exc -> do
                atomically (putTMVar tmvar (Left exc))
                g' <- mkCryptoGen
                loop name q g'
            Right (a, g') -> do
                atomically (putTMVar tmvar (Right a))
                loop name q g'
