{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.HoursApi.Class (
    -- * Monad class
    MonadHours (..),
    -- * Data structures
    Task (..),
    Project (..),
    Timereport (..),
    NewTimereport (..),
    Capacity (..),
    ReportableAssignment (..),
    ) where

import Data.Fixed                (Centi)
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval)
import Prelude ()

import qualified Futurice.App.HoursApi.Types as T
import qualified PlanMill                    as PM

-- | The core interface, in which "Futurice.App.HoursApi.Logic" is written.
--
-- We don't reuse "PlanMill" records, to make mocking simpler
-- (i.e. we omit unrelevant fields).
--
class (MonadTime m, MonadLog m) => MonadHours m where
    -- Volatile things, we probably ask from PlanMill

    -- | Remaining vacations.
    vacationRemaining :: m (NDT 'Days Centi)

    -- | Flex balance.
    flexBalance :: m (NDT 'Hours Centi)

    -- | Working hours per day.
    workingHours :: m (NDT 'Hours Centi)

    -- | Project task.
    task :: PM.TaskId -> m Task

    -- | Project.
    project :: PM.ProjectId -> m Project

    -- | Projects user can report ATM.
    reportableProjects :: PM.UserId -> m [ReportableAssignment]

    -- | Timereports for the interval (inclusive).
    timereports :: Interval Day -> PM.UserId -> m [Timereport]

    -- | Single timereport.
    timereport :: PM.TimereportId -> m Timereport

    -- | Delete timereport
    deleteTimereport :: PM.TimereportId -> m ()

    -- | New timereport
    addTimereport :: NewTimereport -> m ()

    -- | Edit timereport
    editTimereport :: PM.TimereportId -> NewTimereport -> m ()

    -- | Capacities.
    --
    -- Returns a capacity for each day in the interval.
    capacities :: Interval Day -> PM.UserId -> m [Capacity]

    -- Not-so-volatile things!

    -- | Latest entry.
    --
    -- "Best" guess for the entry for the project.
    --
    -- /Note:/ we reuse 'LatestEntry' as a result type.
    latestEntry :: PM.ProjectId -> m (Maybe T.LatestEntry)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data ReportableAssignment = ReportableAssignment
    { _raProjectId :: !PM.ProjectId
    , _raTaskId    :: !PM.TaskId
    , _raFinish    :: !UTCTime
    }
  deriving (Eq, Show, Generic)

data Task = Task {
    }
  deriving (Eq, Show, Generic)

data Project = Project {
    }
  deriving (Eq, Show, Generic)

data Timereport = Timereport {
    }
  deriving (Eq, Show, Generic)

data NewTimereport = NewTimereport {
    }
  deriving (Eq, Show, Generic)

data Capacity = Capacity
    { _capacityDay          :: !Day
    , _capacityAmount       :: !(NDT 'Hours Centi)
    , _capacityDescription  :: !Text  -- ^ name of the day
    }
  deriving (Eq, Show, Generic)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeLenses ''Capacity
makeLenses ''NewTimereport
makeLenses ''Project
makeLenses ''ReportableAssignment
makeLenses ''Task
makeLenses ''Timereport
