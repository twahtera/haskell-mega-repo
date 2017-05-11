{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.HoursMock.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (forOf)
import Data.Fixed                (Centi)
import Futurice.Time             (NDT (..))
import Futurice.Time.Month       (dayToMonth)
import Numeric.Interval.NonEmpty ((...))
import Servant
import System.Random             (randomRIO)

import Futurice.App.HoursApi.Types
import Futurice.App.HoursMock.Ctx
import Futurice.App.HoursMock.MockData

import qualified Data.Map.Strict as Map
import qualified FUM
import qualified PlanMill        as PM
import qualified Test.QuickCheck as QC

projectEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler [Project ReportableTask]
projectEndpoint _ctx _fumUser = liftIO $ do
    QC.sample' QC.arbitrary

-- | @GET /user@
userEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler User
userEndpoint _ctx _fumUser = liftIO $ do
    -- TODO: how to generate unique values per request?
    _userBalance <- fromInteger <$> randomRIO (-10,40)
    _userHolidaysLeft <- randomRIO (0, 24)
    _userUtilizationRate <- randomRIO (0,100) :: IO Float
    pure $ User
        { _userFirstName      = "Test"
        , _userLastName       = "User"
        , _userProfilePicture = "https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
        , ..
        }

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> Handler HoursResponse
hoursEndpoint _ctx _fumUser (Just sd) (Just ed) = do
    months <- liftIO $ fmap last $ QC.sample' $ genMonths [ sd .. ed ]
    p <- liftIO $ fillProjects
    return $ HoursResponse
        { _hoursResponseReportableProjects = p
        , _hoursResponseMarkedProjects     = fromReToMa p -- TODO: temporary
        , _hoursResponseMonths             = months
        , _hoursResponseDefaultWorkHours   = 7.5
        }

-- start or end days not set
hoursEndpoint _ _ _ _ = throwError err400

-- Generate a HoursDay for each day, and then group them into maps
genMonths :: [Day] -> QC.Gen (Map.Map Month HoursMonth)
genMonths ds = do
    -- generate entry for each day
    es <- for ds $ \d -> do
        -- 0-3 entries per day, with one most frequent.
        n <- QC.frequency [(1, pure 0), (1, pure 2), (1, pure 3), (10, pure 1) ]
        es <- QC.vectorOf n (QC.elements entries)

        -- mock days doesn't have day set
        pure $ es & traverse . entryDay .~ d

    -- TODO: there is no holiday map
    pure $ mkHoursMonth (minimum ds ... maximum ds) mempty $ concat es


-- | /TODO/: we should use 'MonadRandom', not 'MonadIO'.
fillProjects :: forall m. (MonadTime m, MonadIO m) => m [Project ReportableTask]
fillProjects = for projects $ \p -> do
    forOf (projectTasks . traverse) p $ \t -> do
        tle <- for (t ^? rtaskLatestEntry . _Just) $ \x -> do
            today <- currentDay
            hrs <- liftIO $ randomRIO (1, 7) :: m Int
            pure $ x
                { _latestEntryDate  = today
                , _latestEntryHours = fromIntegral hrs * 0.5
                }
        hrsRemaining <- case _projectId p /= _projectId internalProject && _projectId p /= _projectId absenceProject of
            True -> do
                x <- liftIO $ randomRIO (-10, 20) :: m Centi
                pure $ Just x
            False -> pure $ Nothing
        pure $ t
            { _rtaskLatestEntry    = tle
            , _rtaskHoursRemaining = NDT <$> hrsRemaining
            }

mkEntryEndPoint :: (MonadTime m, MonadIO m) => EntryUpdate -> m EntryUpdateResponse
mkEntryEndPoint req = do
    ps <- fillProjects
    let date = _euDate req
    usrB <- liftIO $ fromInteger <$> randomRIO (-10, 40)
    usrHL <- liftIO $ randomRIO (0, 24)
    usrUTZ <- liftIO $ randomRIO (0,100)
    newEntryId <- liftIO $ randomRIO (0, 100)
    let md = HoursDay
            { _dayType    = DayTypeNormal
            , _dayHours   = _euHours req
            , _dayClosed  = False
            , _dayEntries = pure $ Entry
                { _entryId          = PM.Ident newEntryId
                , _entryDay         = ModifiedJulianDay 0 -- wrong
                , _entryProjectId   = _euProjectId req
                , _entryTaskId      = _euTaskId req
                , _entryDescription = fromMaybe "" $ _euDescription req
                , _entryHours       = _euHours req
                , _entryClosed      = fromMaybe False (_euClosed req) -- TODO: is Maybe open or closed?
                , _entryBillable    = EntryTypeBillable -- wrong
                }
            }
    let d = Map.fromList [(date, md)]
    let mm = HoursMonth
              { _monthHours=75
            , _monthUtilizationRate=70
            , _monthDays=d}
    let months = Map.fromList [(dayToMonth date, mm)]
    let userResponse = User
            { _userFirstName="Test"
            , _userLastName="User"
            , _userBalance=usrB
            , _userHolidaysLeft=usrHL
            , _userUtilizationRate=usrUTZ
            , _userProfilePicture="https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
            }
    let hoursResponse = HoursResponse
            { _hoursResponseDefaultWorkHours   = 8
            , _hoursResponseReportableProjects = ps
            , _hoursResponseMarkedProjects     = fromReToMa ps -- TODO: temporary
            , _hoursResponseMonths             = months
            }
    pure $ EntryUpdateResponse
        { _eurUser=userResponse
        , _eurHours=hoursResponse
        }

-- | @POST /entry@
entryEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryEndpoint _ctx _fumUser req = do
    res <- liftIO $ mkEntryEndPoint req
    return res

-- | @PUT /entry/#id@
entryIdEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryIdEndpoint _ctx _fumUser _id req = do
    res <- liftIO $ mkEntryEndPoint req
    -- Set every entryId to 1.
    let res' = res
            & eurHoursDayUpdates
            . dayEntries . traverse . entryId .~ PM.Ident 1
    return res'

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> PM.TimereportId
    -> Handler EntryUpdateResponse
entryDeleteEndpoint _ctx _fumUser _id = do
    now <- currentTime
    let dummyReq = EntryUpdate
            { _euTaskId      = PM.Ident 1
            , _euProjectId   = PM.Ident 1
            , _euDescription = Just "test"
            , _euDate        = utctDay now
            , _euHours       = 8
            , _euClosed      = Nothing
            }
    res <- liftIO $ mkEntryEndPoint dummyReq
    let res' = res
            & eurHoursDayUpdates . dayHours .~ 0
            & eurHoursDayUpdates . dayEntries .~ []
    return res'

eurHoursDayUpdates :: Traversal' EntryUpdateResponse HoursDay
eurHoursDayUpdates =
    eurHours . hoursResponseMonths . traverse . monthDays . traverse

-- | Temporary helper to keep mock-api in line with hours-api
fromReToMa :: [Project ReportableTask] -> [Project MarkedTask]
fromReToMa ts = map fn ts
  where
    fn = over (projectTasks. traverse) mkTask'
    mkTask' t = MarkedTask
        { _mtaskId = _rtaskId t
        , _mtaskName = _rtaskName t
        , _mtaskClosed = _rtaskClosed t
        , _mtaskAbsence = False
        }
