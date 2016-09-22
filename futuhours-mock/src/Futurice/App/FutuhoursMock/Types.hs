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
    -- * Ctx
    Ctx (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import qualified PlanMill as PM

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

data Project = Project
    { _projectId     :: PM.ProjectId
    , _projectName   :: !Text
    , _projectTasks     :: [Task]
    , _projectClosed :: !Bool
    }
  deriving (Eq, Show, Typeable, Generic)

data Task = Task
  { _taskId :: PM.TaskId
  , _taskName :: !Text
  , _taskAbsence :: !Bool
  , _taskClosed :: !Bool
  , _taskLatestEntry :: Maybe Entry
  , _taskHoursRemaining :: !Float -- TODO: better type
  } deriving (Eq, Show, Typeable, Generic)
-- LatestEntry: Hours UI feature. Previous Entry used as default values when marking new hours.
-- HoursRemaining: Task.taskTargetEffort - (totalHoursUsedByAssignedPersonnel)

data Entry = Entry
  { _entryId :: PM.TimereportId
  , _entryProjectId :: PM.ProjectId
  , _entrytaskId :: PM.TaskId
  , _entryDate :: !UTCTime
  , _entryDescription :: !Text
  , _entryClosed :: !Bool
  , _entryHours :: !Float
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Project
deriveGeneric ''Project

makeLenses ''Task
deriveGeneric ''Task

makeLenses ''Entry
deriveGeneric ''Entry

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

instance Arbitrary Entry where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON Entry where toJSON = sopToJSON
instance FromJSON Entry where parseJSON = sopParseJSON
instance ToSchema Entry where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
