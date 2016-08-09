{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
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

import Control.Concurrent       (ThreadId, forkIO, threadDelay)
import Data.Time                (addUTCTime, getCurrentTime)

-------------------------------------------------------------------------------
-- Worker
-------------------------------------------------------------------------------

data Options = Options
    { optionsLogger   :: forall a. (forall m. (Applicative m, MonadLogger m, MonadIO m) => m a) -> IO a
    , optionsInterval :: !NominalDiffTime
    }

spawnPeriocron :: Options -> [(Job, Intervals)] -> IO ThreadId
spawnPeriocron options defs = do
    optionsLogger options $ $(logInfo) "Spawning periocron worker"
    now <- getCurrentTime
    let jobs :: [(UTCTime, Job)]
        jobs = map (first (flip addUTCTime now)) $ mergeDefs defs
    forkIO $ workerLoop options jobs

workerLoop :: Options -> [(UTCTime, Job)] -> IO ()
workerLoop options = iterateM $ \jobs -> optionsLogger options $ go jobs
  where
    go :: (Applicative m, MonadLogger m, MonadIO m) => [(UTCTime, Job)] -> m [(UTCTime, Job)]
    go jobs = do
        now <- liftIO $ getCurrentTime
        let (todo, rest) = span ((< now) . fst) jobs

        $(logInfo) $
            "Periocron worker loop at " <> textShow now <>
            "; jobs to do: " <> textShow (length todo)

        -- Execute jobs
        traverse_ (executeJob . snd) todo 
        
        -- Sleep
        liftIO $ threadDelay $ round $ (* 1000000) $ optionsInterval options

        -- Loop
        pure rest

    executeJob :: (Applicative m, MonadLogger m, MonadIO m) => Job -> m ()
    executeJob (Job label action) = do
        $(logDebug) $ "Executing periocron job " <> label
        x <- liftIO $ tryDeep action
        case x of
            Right _  -> pure ()
            Left exc ->
                $(logError) $ "Exception while executing periocron job "
                    <> label <> " -- " <> textShow exc

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
