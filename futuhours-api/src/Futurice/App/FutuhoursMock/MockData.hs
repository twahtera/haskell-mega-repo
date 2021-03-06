{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Futurice.App.FutuhoursMock.MockData (
    projects,
    entries,
    internalProject,
    absenceProject,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens     (Getter, to)

import Futurice.App.FutuhoursApi.Types

import qualified PlanMill as PM

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mkTask' = mkTask . PM.Ident
mkTask' :: Word64 -> Text -> Task

mkLatestEntry' :: Text -> Maybe LatestEntry
mkLatestEntry' = Just . mkLatestEntry

projectFirstTaskId :: PM.TaskId -> Getter Project PM.TaskId
projectFirstTaskId def = to $ \project -> fromMaybe def $
    project ^? projectTasks . traverse . taskId

projectLatestDescription :: Getter Project Text
projectLatestDescription = to $ \project -> fromMaybe "" $
    project ^? projectTasks . traverse . taskLatestEntry .  _Just .  latestEntryDescription

-------------------------------------------------------------------------------
-- Mock data:
-------------------------------------------------------------------------------

internalProject :: Project
internalProject = Project
    { _projectId = PM.Ident 1
    , _projectName = "Internal Work"
    , _projectClosed = False
    , _projectTasks =
        [ mkTask' 1 "Things" & taskLatestEntry .~ mkLatestEntry' "Doing things"
        , mkTask' 2 "Stuff" & taskLatestEntry .~ mkLatestEntry' "Doing stuff"
        ]
    }

absenceProject :: Project
absenceProject = Project
    { _projectId = PM.Ident 4
    , _projectName = "Absences"
    , _projectClosed = False
    , _projectTasks =
        [ mkTask' 6 "Balance leave" & taskLatestEntry .~ mkLatestEntry' "Balance leave"
        , mkTask' 7 "Unpaid holiday" & taskLatestEntry .~ mkLatestEntry' "Unpaid holiday"
        , mkTask' 8 "Sick leave" & taskLatestEntry .~ mkLatestEntry' "Sick leave"
        ]
    }

customerProject :: Project
customerProject = Project
    { _projectId = PM.Ident 2
    , _projectName = "Actual customer work"
    , _projectClosed = False
    , _projectTasks =
        [ mkTask' 3 "Development" & taskLatestEntry .~ mkLatestEntry' "Development"
        , mkTask' 4 "Long weekend :()" & taskLatestEntry .~ mkLatestEntry' "On-Call"
        ]
    }

inactiveProject :: Project
inactiveProject = Project
    { _projectId = PM.Ident 3
    , _projectName = "Not active project"
    , _projectClosed = True
    , _projectTasks =
        [ mkTask' 5 "Doing work" & taskLatestEntry .~ mkLatestEntry' "Work"
        , mkTask' 6 "Designing" & taskLatestEntry .~ mkLatestEntry' "Design"
        ]
    }

projects :: [Project]
projects = [internalProject, absenceProject, customerProject, inactiveProject]

entries :: [Entry]
entries =
    [ Entry
        { _entryId          = PM.Ident 1
        , _entryDay         = ModifiedJulianDay 0
        , _entryProjectId   = internalProject ^. projectId
        , _entryTaskId      = internalProject ^. projectFirstTaskId (PM.Ident 1)
        , _entryDescription = "Internal work"
        , _entryHours       = 5
        , _entryClosed      = False
        , _entryBillable    = EntryTypeNotBillable
        }
    , Entry
        { _entryId          = PM.Ident 2
        , _entryDay         = ModifiedJulianDay 0
        , _entryProjectId   = absenceProject ^. projectId
        , _entryTaskId      = absenceProject ^. projectFirstTaskId (PM.Ident 2)
        , _entryDescription = absenceProject ^. projectLatestDescription
        , _entryHours       = 8
        , _entryClosed      = False
        , _entryBillable    = EntryTypeOther
        }
    , Entry
        { _entryId          = PM.Ident 13
        , _entryDay         = ModifiedJulianDay 0
        , _entryProjectId   = customerProject ^. projectId
        , _entryTaskId      = customerProject ^. projectFirstTaskId (PM.Ident 13)
        , _entryDescription = customerProject ^. projectLatestDescription
        , _entryHours       = 10
        , _entryClosed      = False
        , _entryBillable    = EntryTypeBillable
        }
    , Entry
        { _entryId          = PM.Ident 4
        , _entryDay         = ModifiedJulianDay 0
        , _entryProjectId   = absenceProject ^. projectId
        , _entryTaskId      = absenceProject ^. projectFirstTaskId (PM.Ident 4)
        , _entryDescription = absenceProject ^. projectLatestDescription
        , _entryHours       = 3
        , _entryClosed      = False
        , _entryBillable    = EntryTypeOther
        }
    , Entry
        { _entryId          = PM.Ident 5
        , _entryDay         = ModifiedJulianDay 0
        , _entryProjectId   = customerProject ^. projectId
        , _entryTaskId      = fromMaybe (PM.Ident 5) $ customerProject ^? projectTasks . traverse .  taskId
        , _entryDescription = "Customer work"
        , _entryHours       = 5
        , _entryClosed      = False
        , _entryBillable    = EntryTypeBillable
        }
    , Entry
        { _entryId          = PM.Ident 6
        , _entryDay         = ModifiedJulianDay 0
        , _entryProjectId   = inactiveProject ^. projectId
        , _entryTaskId      = inactiveProject ^. projectFirstTaskId (PM.Ident 6)
        , _entryDescription = inactiveProject ^. projectLatestDescription
        , _entryHours       = 9
        , _entryClosed      = True
        , _entryBillable    = EntryTypeBillable
        }
    , Entry
        { _entryId          = PM.Ident 7
        , _entryDay         = ModifiedJulianDay 0
        , _entryProjectId   = inactiveProject ^. projectId
        , _entryTaskId      = inactiveProject ^. projectFirstTaskId (PM.Ident 7)
        , _entryDescription = inactiveProject ^. projectLatestDescription
        , _entryHours       = 8
        , _entryClosed      = True
        , _entryBillable    = EntryTypeBillable
        }
    ]
