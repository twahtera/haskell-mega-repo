{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.Types (
    -- * Project
    Project (..),
    projectId,
    projectName,
    projectClosed,
    projectTasks,
    -- * Task
    Task (..),
    taskLatestEntry,
    taskId,
    mkTask,
    -- * Entry
    Entry (..),
    LatestEntry (..),
    latestEntryDescription,
    mkLatestEntry,
    -- * User
    User (..),
    -- * Ctx
    Ctx (..),
    -- * Hours
    HoursDay (..),
    mkHoursDay,
    HoursMonth (..),
    HoursResponse (..),
    HoursDayUpdate (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Map (Map)

import qualified PlanMill as PM

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

data Project = Project
    { _projectId     :: PM.ProjectId
    , _projectName   :: !Text
    , _projectTasks  :: [Task]
    , _projectClosed :: !Bool
    }
  deriving (Eq, Show, Typeable, Generic)

-- perhaps a lens getter for an Entry?
data LatestEntry = LatestEntry
  { _latestEntryDescription :: !Text
  , _latestEntryDate :: Maybe UTCTime
  , _latestEntryHours :: Maybe Float
  } deriving (Eq, Show, Typeable, Generic)

mkLatestEntry desc = Just LatestEntry
                        { _latestEntryDescription=desc
                        , _latestEntryDate=Nothing
                        , _latestEntryHours=Nothing
                        }

data Task = Task
  { _taskId :: PM.TaskId
  , _taskName :: !Text
  , _taskAbsence :: !Bool
  , _taskClosed :: !Bool
  , _taskLatestEntry :: Maybe LatestEntry
  , _taskHoursRemaining :: !(Maybe Float) -- TODO: better type
  } deriving (Eq, Show, Typeable, Generic)
-- LatestEntry: Hours UI feature. Previous Entry used as default values when marking new hours.
-- HoursRemaining: Task.taskTargetEffort - (totalHoursUsedByAssignedPersonnel)
-- NOTE: Golang backend LatestEntry is Entry with a Date

mkTask id name = Task {
                    _taskId=PM.Ident id
                  , _taskName=name
                  , _taskAbsence=False
                  , _taskClosed=False
                  , _taskLatestEntry=Nothing
                  , _taskHoursRemaining=Nothing}

-- Entries for a specific Day
data Entry = Entry
  { _entryId :: PM.TimereportId
  , _entryProjectId :: PM.ProjectId
  , _entryTaskId :: PM.TaskId
  , _entryDescription :: !Text
  , _entryClosed :: !Bool
  , _entryHours :: !Float
  } deriving (Eq, Show, Typeable, Generic)

-- Golang: UserResponse
data User = User
  { _userFirstName :: !Text
  , _userLastName :: !Text
  , _userBalance :: !Float
  , _userHolidaysLeft :: !Int
  , _userUtilizationRate :: !Float
  , _userProfilePicture :: !Text
  }

-- filling HolidayName marks Day as a Holiday
data HoursDay = HoursDay
  { _dayHolidayName :: !(Maybe Text)
  , _dayHours :: !Float
  , _dayEntries :: ![Entry]
  , _dayClosed :: !Bool
  }

mkHoursDay = HoursDay
              { _dayHolidayName=Nothing
              , _dayHours=0.0
              , _dayEntries=[]
              , _dayClosed=False
              }

-- HoursDayUpdate is HoursDay with dayClosed::Maybe Bool AND *singular* dayEntries :: !Entry
-- Keep different types now; perhaps refactor UI to lessen Backend types in Future *shrug*
data HoursDayUpdate = HoursDayUpdate
  { _hoursDayUpdateHolidayName :: !Text
  , _hoursDayUpdateHours :: !Float
  , _hoursDayUpdateEntry :: !Entry
  }

data HoursMonth = HoursMonth
  { _monthHours :: !Float
  , _monthUtilizationRate :: !Float
  , _monthDays :: Map Text [HoursDay]
  }

data HoursMonthUpdate = HoursMonthUpdate
  { _hoursMonthUpdateHours :: !Float
  , _hoursMonthUpdateUtilizationRate :: !Float
  , _hoursMonthUpdateDays :: Map Text [HoursDayUpdate]
  }

data HoursResponse = HoursResponse
  { _hoursResponseDefaultWorkHours :: !Float
  , _hoursResponseProjects :: ![Project]
  , _hoursResponseMonths :: Map Text [HoursMonth]
  }

data HoursUpdateResponse = HoursUpdateResponse
  { _hoursUpdateResponseDefaultWorkHours :: !Float
  , _hoursUpdateResponseProjects :: ![Project]
  , _hoursUpdateResponseMonths :: Map Text [HoursMonthUpdate]
  }

makeLenses ''Project
deriveGeneric ''Project

makeLenses ''Task
deriveGeneric ''Task

makeLenses ''LatestEntry
deriveGeneric ''LatestEntry

makeLenses ''Entry
deriveGeneric ''Entry

makeLenses ''User
deriveGeneric ''User

makeLenses ''HoursDay
deriveGeneric ''HoursDay

makeLenses ''HoursMonth
deriveGeneric ''HoursMonth

makeLenses ''HoursResponse
deriveGeneric ''HoursResponse

instance Arbitrary Project where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON Project where toJSON = sopToJSON
instance FromJSON Project where parseJSON = sopParseJSON
instance ToSchema Project where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary Task where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON Task where toJSON = sopToJSON
instance FromJSON Task where parseJSON = sopParseJSON
instance ToSchema Task where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary LatestEntry where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON LatestEntry where toJSON = sopToJSON
instance FromJSON LatestEntry where parseJSON = sopParseJSON
instance ToSchema LatestEntry where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary Entry where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON Entry where toJSON = sopToJSON
instance FromJSON Entry where parseJSON = sopParseJSON
instance ToSchema Entry where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary User where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON User where toJSON = sopToJSON
instance FromJSON User where parseJSON = sopParseJSON
instance ToSchema User where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursDay where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursDay where toJSON = sopToJSON
instance FromJSON HoursDay where parseJSON = sopParseJSON
instance ToSchema HoursDay where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursMonth where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursMonth where toJSON = sopToJSON
instance FromJSON HoursMonth where parseJSON = sopParseJSON
instance ToSchema HoursMonth where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursResponse where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursResponse where toJSON = sopToJSON
instance FromJSON HoursResponse where parseJSON = sopParseJSON
instance ToSchema HoursResponse where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
