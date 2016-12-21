{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
-- | Pediodic execution on @IO@ actions.
module Futurice.Periocron (
    spawnPeriocron,
    Options(..),
    -- * Job
    Job(..),
    -- * Intervals
    Intervals,
    every,
    shifted,
    ) where

import Futurice.Prelude

import Control.Concurrent               (ThreadId, forkIO, threadDelay)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.Time                        (addUTCTime, diffUTCTime)

-------------------------------------------------------------------------------
-- Worker
-------------------------------------------------------------------------------

data Options = Options
    { optionsLogger   :: !Logger
    , optionsInterval :: !NominalDiffTime
    }

spawnPeriocron :: Options -> [(Job, Intervals)] -> IO ThreadId
spawnPeriocron options defs = do
    now <- currentTime
    let jobs :: [(UTCTime, Job)]
        jobs = map (first (flip addUTCTime now)) $ mergeDefs defs
    forkIO $ workerLoop options jobs

workerLoop :: Options -> [(UTCTime, Job)] -> IO ()
workerLoop options
    = runLogT "periocron" (optionsLogger options)
    . flip evalStateT 0
    . iterateM go
  where
    -- N.B. MonadTime is implied by MonadLog
    go :: (Applicative m, MonadLog m, MonadIO m)
        => [(UTCTime, Job)] -> StateT Int m [(UTCTime, Job)]
    go jobs = do
        now <- currentTime
        let (todo, rest) = span ((< now) . fst) jobs

        logLocalDomain "worker-loop" $ logInfo_ $
            "heart beat at " <> textShow now <>
            "; jobs to do: " <> textShow (length todo)

        -- Execute jobs
        traverse_ (executeJob . snd) todo

        -- Sleep
        liftIO $ threadDelay $ round $ (* 1000000) $ optionsInterval options

        -- Loop
        pure rest

    executeJob
        :: (Applicative m, MonadLog m, MonadIO m)
        => Job -> StateT Int m ()
    executeJob (Job label action) = do
        -- jobid
        jobId <- textShow <$> get
        modify' (+1)
        logLocalDomain ("job-" <> jobId) $ do
            -- execute job
            logInfo_ $ "Executing '" <> label <> "'"
            startTime <- currentTime
            x <- liftIO $ tryDeep action
            endTime <- currentTime
            case x of
                Right _  -> pure ()
                Left exc ->
                    logAttention_ $ "Exception -- " <> textShow exc
            let d = diffUTCTime endTime startTime
            logInfo_ $ "Execution took " <> textShow d

-------------------------------------------------------------------------------
-- Job
-------------------------------------------------------------------------------

-- | Job is an @IO@ action with some label
data Job where
    Job :: NFData a => Text -> IO a -> Job

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

type Intervals = [NominalDiffTime]

every :: NominalDiffTime -> Intervals
every interval = iterate (+ interval) 0

shifted :: NominalDiffTime -> Intervals -> Intervals
shifted diff = map (+ diff)

-------------------------------------------------------------------------------
-- Merge helpers
-------------------------------------------------------------------------------

mergeDefs :: [(Job, Intervals)] -> [(NominalDiffTime, Job)]
mergeDefs defs = mergeManyOn fst defs'
  where
    defs' :: [[(NominalDiffTime, Job)]]
    defs' = map t defs
    t (job, intervals) = map (,job) intervals

mergeOn :: forall a b. Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn f xs ys = go xs' ys'
  where
    xs' = map f' xs
    ys' = map f' ys
    f' x = (x, f x)

    go :: [(a, b)] -> [(a, b)] -> [a]
    go [] bs = map fst bs
    go as [] = map fst as

    go as@((a, a') : as') bs@((b, b') : bs')
        | a' < b'   = a : go as' bs
        | otherwise = b : go as  bs'

mergeManyOn :: forall a b f. (Foldable f, Ord b) => (a -> b) -> f [a] -> [a]
mergeManyOn f = foldr (mergeOn f) []
