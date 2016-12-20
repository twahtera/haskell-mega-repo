{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- |
-- HoursRemaining: Task.taskTargetEffort - (totalHoursUsedByAssignedPersonnel)
--
-- /NOTE:/ Golang backend LatestEntry is Entry with a Date
module Futurice.App.FutuhoursApi.Types where

import Prelude ()
import Futurice.Prelude
import Control.Lens        (Getter, andOf, foldOf, imap, sumOf, to)
import Data.Aeson          (Value (..), withText)
import Data.Swagger        (NamedSchema (..))
import Futurice.Generics
import Futurice.Monoid     (Average (..))
import Futurice.Time.Month (dayToMonth)
import Test.QuickCheck     (arbitraryBoundedEnum)

import qualified Data.Map as Map
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

data Task = Task
    { _taskId             :: PM.TaskId
    , _taskName           :: !Text
    , _taskAbsence        :: !Bool
    , _taskClosed         :: !Bool
    , _taskLatestEntry    :: !(Maybe LatestEntry)
    , _taskHoursRemaining :: !(Maybe Float) -- TODO: better type
    }
  deriving (Eq, Show, Typeable, Generic)

mkTask :: PM.TaskId -> Text -> Task
mkTask i name = Task
    { _taskId             = i
    , _taskName           = name
    , _taskAbsence        = False
    , _taskClosed         = False
    , _taskLatestEntry    = Nothing
    , _taskHoursRemaining = Nothing
    }

-- | Entry may be billable, not billable, or not-countable (i.e. absences)
data EntryType
    = EntryTypeBillable
    | EntryTypeNotBillable
    | EntryTypeOther
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Generic)

-- Entries for a specific Day
data Entry = Entry
    { _entryId          :: !PM.TimereportId
    , _entryProjectId   :: !PM.ProjectId
    , _entryTaskId      :: !PM.TaskId
    , _entryDay         :: !Day
    , _entryDescription :: !Text
    , _entryClosed      :: !Bool
    , _entryHours       :: !Float
    , _entryBillable    :: !EntryType
    }
  deriving (Eq, Show, Typeable, Generic)

entryUtilizationAvg :: Getter Entry (Maybe (Average Float))
entryUtilizationAvg = to $ \entry -> case _entryBillable entry of
    EntryTypeBillable    -> Just $ Average (_entryHours entry) 100
    EntryTypeNotBillable -> Just $ Average (_entryHours entry) 0
    EntryTypeOther       -> Nothing

-- TODO: perhaps a lens getter for an Entry?

-- | Hours UI feature. Previous Entry used as default values when marking new hours.
data LatestEntry = LatestEntry
    { _latestEntryDescription :: !Text
    , _latestEntryDate        :: !(Maybe Day)
    , _latestEntryHours       :: !(Maybe Float)
    }
  deriving (Eq, Show, Typeable, Generic)

mkLatestEntry :: Text -> LatestEntry
mkLatestEntry desc = LatestEntry
    { _latestEntryDescription=desc
    , _latestEntryDate=Nothing
    , _latestEntryHours=Nothing
    }

-- | When frontend sends closed entry to be updated, API doesn't do anything, just respond ok
--
-- /TODO:/ is this *new* entry, probably we don't need closed field at all?
data EntryUpdate = EntryUpdate
    { _euTaskId      :: PM.TaskId
    , _euProjectId   :: PM.ProjectId
    , _euDescription :: !Text
    , _euDate        :: !Day
    , _euHours       :: !Float
    , _euClosed      :: !(Maybe Bool)
    }
  deriving (Eq, Show, Typeable, Generic)

data EntryUpdateResponse = EntryUpdateResponse
    { _eurUser  :: !User
    , _eurHours :: !HoursUpdateResponse
    }
  deriving (Eq, Show, Typeable, Generic)

-- | Golang: UserResponse
data User = User
    { _userFirstName       :: !Text
    , _userLastName        :: !Text
    , _userBalance         :: !Float -- ^ TODO NDT 'Hours Centi
    , _userHolidaysLeft    :: !Int
    , _userUtilizationRate :: !Float
    , _userProfilePicture  :: !Text
    }
  deriving (Eq, Show, Typeable, Generic)

-- |
--
-- /Note:/ filling HolidayName marks Day as a Holiday
--
-- TODO: is it UI feature?
data HoursDay = HoursDay
    { _dayHolidayName :: !(Maybe Text)
    , _dayHours       :: !Float
    , _dayEntries     :: ![Entry]
    , _dayClosed      :: !Bool -- ^ TODO: Maybe Bool, why maybe?
    }
  deriving (Eq, Show, Typeable, Generic)

defaultHoursDay :: HoursDay
defaultHoursDay = HoursDay
    { _dayHolidayName = Nothing
    , _dayHours       = 0.0
    , _dayEntries     = []
    , _dayClosed      = False
    }

-- HoursDayUpdate is HoursDay with dayClosed::Maybe Bool AND *singular* dayEntries :: !Entry
-- Keep different types now; perhaps refactor UI to lessen Backend types in Future *shrug*
data HoursDayUpdate = HoursDayUpdate
    { _hoursDayUpdateHolidayName :: !(Maybe Text)
    , _hoursDayUpdateHours       :: !Float
    , _hoursDayUpdateEntry       :: !(Maybe Entry)
    }
  deriving (Eq, Show, Typeable, Generic)

-- | TODO: add a '_samples' of Utilisation rate weighted average.
data HoursMonth = HoursMonth
    { _monthHours           :: !Float
    , _monthUtilizationRate :: !Float
    , _monthDays            :: Map Day HoursDay -- ^ invariant days of the same month
    }
  deriving (Eq, Show, Typeable, Generic)

data HoursMonthUpdate = HoursMonthUpdate
    { _hoursMonthUpdateHours           :: !Float
    , _hoursMonthUpdateUtilizationRate :: !Float
    , _hoursMonthUpdateDays            :: Map Day [HoursDayUpdate] -- TODO: check invariant on JSON unserialisation
    }
  deriving (Eq, Show, Typeable, Generic)

data HoursResponse = HoursResponse
    { _hoursResponseDefaultWorkHours :: !Float
    , _hoursResponseProjects         :: ![Project]
    , _hoursResponseMonths           :: Map Month HoursMonth -- invariant contents: 'HoursMonth' contains days of key-month
    }
  deriving (Eq, Show, Typeable, Generic)

data HoursUpdateResponse = HoursUpdateResponse
    { _hoursUpdateResponseDefaultWorkHours :: !Float
    , _hoursUpdateResponseProjects         :: ![Project]
    , _hoursUpdateResponseMonths           :: Map Month [HoursMonthUpdate] -- TODO: check invariant on JSON unserialisation
    }
  deriving (Eq, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- Generics and Lenses
-------------------------------------------------------------------------------

makeLenses ''Project
deriveGeneric ''Project

makeLenses ''Task
deriveGeneric ''Task

makeLenses ''LatestEntry
deriveGeneric ''LatestEntry

makeLenses ''Entry
deriveGeneric ''Entry

makeLenses ''EntryUpdate
deriveGeneric ''EntryUpdate

makeLenses ''EntryUpdateResponse
deriveGeneric ''EntryUpdateResponse

makeLenses ''User
deriveGeneric ''User

makeLenses ''HoursDay
deriveGeneric ''HoursDay

makeLenses ''HoursMonth
deriveGeneric ''HoursMonth

makeLenses ''HoursDayUpdate
deriveGeneric ''HoursDayUpdate

makeLenses ''HoursMonthUpdate
deriveGeneric ''HoursMonthUpdate

makeLenses ''HoursResponse
deriveGeneric ''HoursResponse

makeLenses ''HoursUpdateResponse
deriveGeneric ''HoursUpdateResponse

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

-- here because we want to use lenses.

-- | Smart constructor.
mkHoursMonth
    :: Map Day Text  -- ^ Holiday names
    -> [Entry]
    -> Map Month HoursMonth
mkHoursMonth holidays entries =
    let entriesByDay :: Map Day [Entry]
        entriesByDay = Map.fromListWith (++)
            [ (_entryDay e, [e]) | e <- entries ]

        entriesByMonth :: Map Month (Map Day [Entry])
        entriesByMonth = Map.fromListWith (Map.unionWith (++))
            [ (dayToMonth d, Map.singleton d es)
            | (d, es) <- Map.toList entriesByDay
            ]

    in mkHoursMonth' <$> entriesByMonth
  where
    mkHoursMonth' :: Map Day [Entry] -> HoursMonth
    mkHoursMonth' mm = HoursMonth
        { _monthHours           = sumOf (folded . folded . entryHours) mm
        , _monthUtilizationRate = utz
        , _monthDays            = imap mkHoursDay mm
        }
      where
        Average _hours utz = foldOf
            (folded . folded . entryUtilizationAvg . folded)
            mm

    -- Invariant: all entries are on the first day
    mkHoursDay :: Day -> [Entry] -> HoursDay
    mkHoursDay d es = HoursDay
        { _dayHolidayName = holidays ^? ix d
        , _dayHours       = sumOf (folded . entryHours) es
        , _dayEntries     = es
        , _dayClosed      = andOf (folded . entryClosed) es
          -- day is closed if every entry in it is closed
        }

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Arbitrary EntryType where
    arbitrary = arbitraryBoundedEnum
    shrink EntryTypeBillable = []
    shrink et                = [ EntryTypeBillable .. pred et ]

entryTypeText :: EntryType -> Text
entryTypeText EntryTypeBillable    = "billable"
entryTypeText EntryTypeNotBillable = "non-billable"
entryTypeText EntryTypeOther       = "other"

instance ToJSON EntryType where
    toJSON = String . entryTypeText

instance FromJSON EntryType where
    parseJSON = withText "EntryType" $ \t -> maybe
        (fail $ "Invalid entry type: " <> t ^. unpacked)
        pure
        (lookup t m)
      where
        m = [ (entryTypeText et, et) | et <- [ minBound .. maxBound ] ]

instance ToSchema EntryType where
    declareNamedSchema _ = pure $ NamedSchema (Just "EntryType") mempty

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

instance Arbitrary EntryUpdate where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON EntryUpdate where toJSON = sopToJSON
instance FromJSON EntryUpdate where parseJSON = sopParseJSON
instance ToSchema EntryUpdate where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary EntryUpdateResponse where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON EntryUpdateResponse where toJSON = sopToJSON
instance FromJSON EntryUpdateResponse where parseJSON = sopParseJSON
instance ToSchema EntryUpdateResponse where declareNamedSchema = sopDeclareNamedSchema

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

instance Arbitrary HoursDayUpdate where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursDayUpdate where toJSON = sopToJSON
instance FromJSON HoursDayUpdate where parseJSON = sopParseJSON
instance ToSchema HoursDayUpdate where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursMonth where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursMonth where toJSON = sopToJSON
instance FromJSON HoursMonth where parseJSON = sopParseJSON
instance ToSchema HoursMonth where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursMonthUpdate where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursMonthUpdate where toJSON = sopToJSON
instance FromJSON HoursMonthUpdate where parseJSON = sopParseJSON
instance ToSchema HoursMonthUpdate where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursResponse where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursResponse where toJSON = sopToJSON
instance FromJSON HoursResponse where parseJSON = sopParseJSON
instance ToSchema HoursResponse where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursUpdateResponse where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursUpdateResponse where toJSON = sopToJSON
instance FromJSON HoursUpdateResponse where parseJSON = sopParseJSON
instance ToSchema HoursUpdateResponse where declareNamedSchema = sopDeclareNamedSchema
