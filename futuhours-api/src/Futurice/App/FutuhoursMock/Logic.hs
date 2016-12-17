{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.FutuhoursMock.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    mkEntryEndPoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    fillProjects,
    parseDayFormat,
    parseMonthFormat,
    daysFD,
    monthForDay,
    dayInMonth,
    genMonths,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens                        (Traversal', forOf)
import Control.Monad.State.Strict
       (MonadState (..), evalStateT, modify')
import Data.Maybe                          (fromJust)
import Data.Text                           (pack, unpack)
import Data.Time                           (defaultTimeLocale)
import Data.Time.Calendar
       (addDays, diffDays, fromGregorian, toGregorian)
import Data.Time.Format                    (formatTime, parseTimeOrError)
import Data.Vector.Lens                    (vector)
import Futurice.App.FutuhoursMock.MockData
import Futurice.App.FutuhoursMock.Types
import Servant
import System.Random                       (randomRIO)
import Test.QuickCheck                     (arbitrary, sample')

import qualified Data.Map.Strict as Map
import qualified PlanMill        as PM

projectEndpoint :: Ctx -> IO (Vector Project)
projectEndpoint _ctx = do
    p <- sample' arbitrary
    pure $ p ^. vector

-- | @GET /user@
userEndpoint :: Ctx -> IO (User)
userEndpoint _ctx = do
    -- TODO: how to generate unique values per request?
    _userBalance <- randomRIO (-10,40) :: IO Float
    _userHolidaysLeft <- randomRIO (0, 24)
    _userUtilizationRate <- randomRIO (0,100) :: IO Float
    pure $ User
        { _userFirstName      = "Test"
        , _userLastName       = "User"
        , _userProfilePicture = "https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
        , ..
        }

-- TODO: remove
parseDayFormat :: Text -> Day
parseDayFormat x = utctDay $ (parseTimeOrError False defaultTimeLocale "%Y-%m-%d" (unpack x) :: UTCTime)

-- TODO: remove
parseMonthFormat :: Text -> Day
parseMonthFormat x = utctDay $ (parseTimeOrError False defaultTimeLocale "%Y-%m" (unpack x) :: UTCTime)

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe Day
    -> Maybe Day
    -> ExceptT ServantErr IO HoursResponse
hoursEndpoint _ctx sd ed = do
    let duration = diffDays (fromJust sd) (fromJust ed)
    let d = daysFD duration $ (fromJust sd)
    months <- liftIO $ genMonths d
    p <- liftIO $ fillProjects
    return $ HoursResponse
        { _hoursResponseProjects         = p
        , _hoursResponseMonths           = months
        , _hoursResponseDefaultWorkHours = 7.5
        }

daysFD :: Integer -> Day -> [Day]
daysFD duration start = [addDays i start | i <- [0..duration]]

monthForDay :: Day -> String
monthForDay x = formatTime defaultTimeLocale "%Y-%m" x

dayInMonth :: String -> Day -> Bool
dayInMonth m d = m == formatTime defaultTimeLocale "%Y-%m" d

plusOne :: MonadState Int m => m Int
plusOne = do
    val <- get
    modify' (+1)
    pure val

mkGo :: MonadState Int m => m HoursDay
mkGo = do
    newval <- plusOne
    pure $ days !! mod newval (length days)

genMonths :: forall m. MonadIO m => [Day] -> m (Map.Map Text HoursMonth)
genMonths ds = flip evalStateT 1 $ fmap Map.fromList $ do
    for ds $ \mm -> do
        let m     = monthForDay mm
        let days' = [ d | d <- ds, dayInMonth m d ]
        hrs <- liftIO $ randomRIO (0, 150)
        utz <- liftIO $ randomRIO (0, 100)
        days'' <- for days' $ \d' -> do
            let (ym',mm',_) = toGregorian mm
            let (_,_,dd')   = toGregorian d'
            fsd <- mkGo
            pure $ (textShow $ fromGregorian ym' mm' dd', fsd)
        pure $ (,) (pack m) $ HoursMonth
            { _monthHours           = hrs * 0.5
            , _monthUtilizationRate = utz
            , _monthDays            = Map.fromList days''
            }

-- | /TODO/: we should use 'MonadRandom', not 'MonadIO'.
fillProjects :: forall m. (MonadTime m, MonadIO m) => m [Project]
fillProjects = for projects $ \p -> do
    forOf (projectTasks . traverse) p $ \t -> do
        tle <- for (t ^? taskLatestEntry . _Just) $ \x -> do
            today <- currentDay
            hrs <- liftIO $ randomRIO (1, 7) :: m Int
            pure $ x
                { _latestEntryDate  = Just today
                , _latestEntryHours = Just $ fromIntegral hrs * 0.5
                }
        hrsRemaining <- case _projectId p /= _projectId internalProject && _projectId p /= _projectId absenceProject of
            True -> do
                x <- liftIO $ randomRIO (-10, 20) :: m Float
                pure $ Just x
            False -> pure $ Nothing
        pure $ t
            { _taskLatestEntry    = tle
            , _taskHoursRemaining = hrsRemaining
            }

mkEntryEndPoint :: (MonadTime m, MonadIO m) => EntryUpdate -> m EntryUpdateResponse
mkEntryEndPoint req = do
    ps <- fillProjects
    let date = _euDate req
    usrB <- liftIO $ randomRIO (-10, 40)
    usrHL <- liftIO $ randomRIO (0, 24)
    usrUTZ <- liftIO $ randomRIO (0,100)
    newEntryId <- liftIO $ randomRIO (0, 100)
    let md = HoursDayUpdate
            { _hoursDayUpdateHolidayName = Nothing
            , _hoursDayUpdateHours       = _euHours req
            , _hoursDayUpdateEntry       = Just Entry
                { _entryId          = PM.Ident newEntryId
                , _entryProjectId   = _euProjectId req
                , _entryTaskId      = _euTaskId req
                , _entryDescription = _euDescription req
                , _entryHours       = _euHours req
                , _entryClosed      = fromMaybe False (_euClosed req) -- TODO: is Maybe open or closed?
                }
            }
    let d = Map.fromList [(pack $ formatTime defaultTimeLocale "%Y-%m-%d" date, [md])]
    let mm = HoursMonthUpdate
              { _hoursMonthUpdateHours=75
            , _hoursMonthUpdateUtilizationRate=70
            , _hoursMonthUpdateDays=d}
    let months = Map.fromList [(pack $ formatTime defaultTimeLocale "%Y-%m" date, [mm])]
    let userResponse = User
            { _userFirstName="Test"
            , _userLastName="User"
            , _userBalance=usrB
            , _userHolidaysLeft=usrHL
            , _userUtilizationRate=usrUTZ
            , _userProfilePicture="https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
            }
    let hoursResponse = HoursUpdateResponse
            { _hoursUpdateResponseDefaultWorkHours = 7.5
            , _hoursUpdateResponseProjects         = ps
            , _hoursUpdateResponseMonths           = months }
    pure $ EntryUpdateResponse
        { _eurUser=userResponse
        , _eurHours=hoursResponse
        }

-- | @POST /entry@
entryEndpoint
    :: Ctx
    -> EntryUpdate
    -> ExceptT ServantErr IO EntryUpdateResponse
entryEndpoint _ctx req = do
    res <- liftIO $ mkEntryEndPoint req
    return res

-- | @PUT /entry/#id@
entryIdEndpoint
  :: Ctx
  -> Int
  -> EntryUpdate
  -> ExceptT ServantErr IO EntryUpdateResponse
entryIdEndpoint _ctx _id req = do
    res <- liftIO $ mkEntryEndPoint req
    -- Set every entryId to 1.
    let res' = res
            & eurHoursDayUpdates
            . hoursDayUpdateEntry . traverse . entryId .~ PM.Ident 1
    return res'

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Int
    -> EntryUpdate
    -> ExceptT ServantErr IO EntryUpdateResponse
entryDeleteEndpoint _ctx _id _ = do
    now <- currentTime
    let dummyReq = EntryUpdate
            { _euTaskId      = PM.Ident 1
            , _euProjectId   = PM.Ident 1
            , _euDescription = "test"
            , _euDate        = utctDay now
            , _euHours       = 7.5
            , _euClosed      = Nothing
            }
    res <- liftIO $ mkEntryEndPoint dummyReq
    let res' = res
            & eurHoursDayUpdates . hoursDayUpdateHours .~ 0
            & eurHoursDayUpdates . hoursDayUpdateEntry .~ Nothing
    return res'

eurHoursDayUpdates :: Traversal' EntryUpdateResponse HoursDayUpdate
eurHoursDayUpdates
    = eurHours . hoursUpdateResponseMonths . traverse . traverse
    . hoursMonthUpdateDays . traverse . traverse
