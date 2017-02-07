{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.FutuhoursMock.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens        (forOf)
import Data.Vector.Lens    (vector)
import Futurice.Time.Month (dayToMonth)
import Servant
import System.Random       (randomRIO)

import Futurice.App.FutuhoursApi.Types
import Futurice.App.FutuhoursMock.Ctx
import Futurice.App.FutuhoursMock.MockData

import qualified Data.Map.Strict as Map
import qualified FUM
import qualified PlanMill        as PM
import qualified Test.QuickCheck as QC

projectEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler (Vector Project)
projectEndpoint _ctx _fumUser = liftIO $ do
    p <- QC.sample' QC.arbitrary
    pure $ p ^. vector

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
        { _hoursResponseProjects         = p
        , _hoursResponseMonths           = months
        , _hoursResponseDefaultWorkHours = 7.5
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
    pure $ mkHoursMonth mempty $ concat es


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
    usrB <- liftIO $ fromInteger <$> randomRIO (-10, 40)
    usrHL <- liftIO $ randomRIO (0, 24)
    usrUTZ <- liftIO $ randomRIO (0,100)
    newEntryId <- liftIO $ randomRIO (0, 100)
    let md = HoursDayUpdate
            { _hoursDayUpdateHolidayName = Nothing
            , _hoursDayUpdateHours       = _euHours req
            , _hoursDayUpdateEntry       = Just Entry
                { _entryId          = PM.Ident newEntryId
                , _entryDay         = ModifiedJulianDay 0 -- wrong
                , _entryProjectId   = _euProjectId req
                , _entryTaskId      = _euTaskId req
                , _entryDescription = _euDescription req
                , _entryHours       = _euHours req
                , _entryClosed      = fromMaybe False (_euClosed req) -- TODO: is Maybe open or closed?
                , _entryBillable    = EntryTypeBillable -- wrong
                }
            }
    let d = Map.fromList [(date, [md])]
    let mm = HoursMonthUpdate
              { _hoursMonthUpdateHours=75
            , _hoursMonthUpdateUtilizationRate=70
            , _hoursMonthUpdateDays=d}
    let months = Map.fromList [(dayToMonth date, [mm])]
    let userResponse = User
            { _userFirstName="Test"
            , _userLastName="User"
            , _userBalance=usrB
            , _userHolidaysLeft=usrHL
            , _userUtilizationRate=usrUTZ
            , _userProfilePicture="https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
            }
    let hoursResponse = HoursUpdateResponse
            { _hoursUpdateResponseDefaultWorkHours = 8
            , _hoursUpdateResponseProjects         = ps
            , _hoursUpdateResponseMonths           = months }
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
    -> Int
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryIdEndpoint _ctx _fumUser _id req = do
    res <- liftIO $ mkEntryEndPoint req
    -- Set every entryId to 1.
    let res' = res
            & eurHoursDayUpdates
            . hoursDayUpdateEntry . traverse . entryId .~ PM.Ident 1
    return res'

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Int
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryDeleteEndpoint _ctx _fumUser _id _eu = do
    now <- currentTime
    let dummyReq = EntryUpdate
            { _euTaskId      = PM.Ident 1
            , _euProjectId   = PM.Ident 1
            , _euDescription = "test"
            , _euDate        = utctDay now
            , _euHours       = 8
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
