{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Types user in checklist logic.
--
-- Currently missing:
--
-- * How to model construction and modification of 'CheckList'. We want to pick
--   only needed 'Task's based on initial 'User' data, and that information
--   should be configurable dynamically.
module Futurice.App.Checklist.Types (
    -- * Core types
    -- ** User / employee
    User(..),
    ContractType(..),
    Location(..),
    FUMLogin(..),
    -- ** Tasks
    Task(..),
    TaskRole(..),
    CheckResult(..),
    Checklist(..),
    TaskItem(..),
    TaskAppliance(..),
    -- ** Wrappers
    Identifier(..),
    HasIdentifier (..),
    Name (..),
    HasTaskName (..),
    -- * Lenses
    -- ** User
    userFirstName, userLastName, userContractType, userLocation, userConfirmed,
    userPhone, userContactEmail, userStartingDay, userSupervisor, userTribe,
    userInfo, userFUMLogin, userHRNumber,
    -- ** ContractType
    _ContractTypePermanent, _ContractTypeExternal, _ContractTypeFixedTerm,
    _ContractTypePartTimer, _ContractTypeSummerWorker,
    -- ** Location
    _LocHelsinki, _LocTampere, _LocBerlin, _LocLondon,
    _LocStockholm, _LocMunich, _LocOther,
    -- ** Task
    taskCanBeDone, taskDependencies, taskCheck, taskRole,
    -- ** CheckResult
    _CheckResultSuccess, _CheckResultMaybe, _CheckResultFailure,
    -- ** TaskRole
    _TaskRoleIT, _TaskRoleHR, _TaskRoleSupervisor, _TaskRoleOther,
    -- ** Checklist
    checklistName, checklistTasks,
    -- ** TaskItem
    taskItemUser, taskItemTask, taskItemDone,
    -- * HTML
    Page (..),
    -- * World
    World,
    mkWorld,
    worldValid,
    ) where

import Futurice.App.Checklist.Types.Basic
import Futurice.App.Checklist.Types.Page
import Futurice.App.Checklist.Types.World
