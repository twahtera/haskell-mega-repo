{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    fillProjects,
    parseDayFormat,
    parseMonthFormat,
    daysFD,
    monthsForDays,
    dayInMonth,
    genMonths,
    ) where

import Futurice.App.FutuhoursMock.Types
import Futurice.Prelude
import Prelude ()

import Data.IORef
import Data.Vector.Lens (vector)
import Data.Text (pack, unpack)
import Futurice.App.FutuhoursMock.MockData
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdRandom, randomR, randomRIO)
import Test.QuickCheck (arbitrary, sample')
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (parseTimeOrError, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.Calendar (diffDays, addDays, showGregorian)
import Data.Maybe (fromJust)
import Futurice.Servant
import Servant
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import System.IO                  (hPutStrLn, stderr)
import qualified Data.Map.Strict as Map


projectEndpoint :: Ctx -> IO (Vector Project)
projectEndpoint _ctx = do
    projects <- sample' arbitrary
    pure $ projects ^. vector

userEndpoint :: Ctx -> IO (User)
userEndpoint _ctx = do
  -- GET /user
    -- TODO: how to generate unique values per request?
    _userBalance <- getStdRandom (randomR (-10,40)) :: IO Float
    _userHolidaysLeft <- randomRIO (0, 24)
    _userUtilizationRate <- getStdRandom (randomR (0,100)) :: IO Float
    pure $ User
        { _userFirstName="Test"
        , _userLastName="User"
        , _userBalance=_userBalance
        , _userHolidaysLeft=_userHolidaysLeft
        , _userUtilizationRate=_userUtilizationRate
        , _userProfilePicture="https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
        }

parseDayFormat x = (parseTimeOrError False defaultTimeLocale "%Y-%m-%d" (unpack x) :: UTCTime)
parseMonthFormat x = (parseTimeOrError False defaultTimeLocale "%Y-%m" (unpack x) :: UTCTime)

hoursEndpoint
  :: Ctx
  -> Maybe Text
  -> Maybe Text
  -> ExceptT ServantErr IO HoursResponse
hoursEndpoint _ctx sd ed = do
  -- GET /hours
  let startDate = case sd of
                    Just x -> Just $ parseDayFormat x
                    Nothing -> Nothing
  let endDate = case ed of
                  Just x -> Just $ parseDayFormat x
                  Nothing -> Nothing
  let duration = diffDays (utctDay $ fromJust endDate) (utctDay $ fromJust startDate)
  let days = daysFD duration $ utctDay (fromJust startDate)
  months <- liftIO $ genMonths days
  projects <- liftIO $ fillProjects
  return $ HoursResponse { _hoursResponseProjects=projects
                         , _hoursResponseMonths=months
                         , _hoursResponseDefaultWorkHours=7.5
                         }

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
  r <- newIORef 0
  return $ \i -> do
    modifyIORef' r (+i)
    readIORef r

daysFD :: Integer -> Day -> [Day]
daysFD duration start = [addDays i start | i <- [0..duration]]

monthsForDays :: [Day] -> [String]
monthsForDays xs = nub [formatTime defaultTimeLocale "%Y-%m" x | x <- xs]

dayInMonth :: String -> Day -> Bool
dayInMonth m d = (m == formatTime defaultTimeLocale "%Y-%m" d)

genMonths :: [Day] -> IO (Map.Map Text HoursMonth)
genMonths ds = do
  c <- makeCounter
  ms <- flip traverse (monthsForDays ds) $ \m -> do
    let go = \i -> days !! (mod (unsafePerformIO $ c i) (length days))
    let days = [go 1 | d<-ds, dayInMonth m d]
    hrs <- randomRIO (0, 150) :: IO Int
    utz <- getStdRandom (randomR (0,100)) :: IO Float
    pure $ (pack m, HoursMonth
                    { _monthHours=(fromIntegral hrs)*0.5
                    , _monthUtilizationRate=utz
                    , _monthDays=Map.fromList [(pack m, days)]})
  pure $ Map.fromList ms

fillProjects :: IO [Project]
fillProjects = do
  ps' <- flip traverse projects $ \p -> do
    ts' <- flip traverse (p ^.. projectTasks . traverse) $ \t -> do
      now <- getCurrentTime
      hrs <- randomRIO (1, 7) :: IO Int
      let t'' = case (fromMaybe Nothing $ t ^? taskLatestEntry) of
                  Just x -> Just $ x { _latestEntryDate = Just now
                                     , _latestEntryHours = Just $ (fromIntegral hrs)*0.5 }
                  Nothing -> Nothing
      hrsRemaining <- case (_projectId p /= _projectId internalProject && _projectId p /= _projectId absenceProject) of
                          True -> do
                            x <- randomRIO (-10, 20) :: IO Float
                            pure $ Just x
                          False -> pure $ Nothing
      pure $ t { _taskLatestEntry=t'', _taskHoursRemaining=hrsRemaining }
    pure $ p { _projectTasks=ts'}
  pure ps'

entryEndpoint
  :: Ctx
  -> EntryUpdate
  -> ExceptT ServantErr IO HoursUpdateResponse
entryEndpoint _ctx req = do
  -- POST /entry
  projects <- liftIO $ fillProjects
  pure $ HoursUpdateResponse { _hoursUpdateResponseDefaultWorkHours=7.5
                             , _hoursUpdateResponseProjects=[]
                             , _hoursUpdateResponseMonths=Map.empty}

entryIdEndpoint :: Ctx -> Int -> IO ([Int])
entryIdEndpoint _ctx _id = do
  -- PUT /entry/#id
  pure $ [1]

entryDeleteEndpoint :: Ctx -> Int -> IO ([Int])
entryDeleteEndpoint _ctx _id = do
  -- DELETE /entry/#id
  pure $ [1]
