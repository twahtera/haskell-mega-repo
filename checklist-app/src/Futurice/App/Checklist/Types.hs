{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Types employee in checklist logic.
--
-- Currently missing:
--
-- * How to model construction and modification of 'CheckList'. We want to pick
--   only needed 'Task's based on initial 'Employee' data, and that information
--   should be configurable dynamically.
module Futurice.App.Checklist.Types (
    -- * Core types
    -- ** Employee / employee
    Employee(..),
    ContractType(..),
    Location(..),
    -- ** Tasks
    Task(..),
    TaskRole(..),
    CheckResult(..),
    Checklist(..),
    TaskItem (..),
    TaskAppliance(..),
    -- ** Wrappers
    Identifier(..),
    identifierToText,
    HasIdentifier (..),
    identifierText,
    Name (..),
    HasName (..),
    -- * Functions
    employeeTaskApplies,
    -- * Lenses
    -- ** Employee
    employeeFirstName, employeeLastName, employeeContractType, employeeLocation, employeeConfirmed,
    employeePhone, employeeContactEmail, employeeStartingDay, employeeSupervisor, employeeTribe,
    employeeInfo, employeeFUMLogin, employeeHRNumber, employeeChecklist,
    -- ** ContractType
    _ContractType,
    _ContractTypePermanent, _ContractTypeExternal, _ContractTypeFixedTerm,
    _ContractTypePartTimer, _ContractTypeSummerWorker,
    -- ** Location
    _Location,
    _LocHelsinki, _LocTampere, _LocBerlin, _LocLondon,
    _LocStockholm, _LocMunich, _LocOther,
    locationToText, locationFromText,
    -- ** Task
    taskName, taskInfo, taskPrereqs, taskRole,
    -- ** CheckResult
    _CheckResultSuccess, _CheckResultMaybe, _CheckResultFailure,
    -- ** TaskRole
    _TaskRole,
    _TaskRoleIT, _TaskRoleHR, _TaskRoleSupervisor,
    taskRoleToText, taskRoleFromText,
    -- ** Checklist
    checklistName, checklistTasks,
    -- ** TaskItemDone
    _TaskItem,
    _TaskItemDone, _TaskItemTodo,
    -- * World
    World,
    emptyWorld,
    mkWorld,
    -- ** Lenses
    worldEmployees,
    worldTasks,
    worldLists,
    worldTaskItems,
    worldUsers,
    worldTaskItems',
    worldTasksSorted,
    -- * Access
    AuthUser,
    ) where

import Futurice.App.Checklist.Types.Basic
import Futurice.App.Checklist.Types.ContractType
import Futurice.App.Checklist.Types.Identifier
import Futurice.App.Checklist.Types.Location
import Futurice.App.Checklist.Types.TaskItem
import Futurice.App.Checklist.Types.World
import Futurice.App.Checklist.Types.TaskAppliance
import Futurice.App.Checklist.Types.TaskRole

import qualified FUM (UserName)

type AuthUser = (FUM.UserName, TaskRole, Location)
