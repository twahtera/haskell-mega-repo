{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
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
    CheckCondition(..),
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
    -- ** CheckCondition
    _CheckConditionSuccess, _CheckConditionMaybe, _CheckConditionFailure,
    -- ** TaskRole
    _TaskRoleIT, _TaskRoleHR, _TaskRoleSupervisor, _TaskRoleOther,
    -- ** Checklist
    checklistName, checklistTasks,
    -- ** TaskItem
    taskItemUser, taskItemTask, taskItemDone,
    ) where

import Futurice.Prelude
import Prelude ()

-- Should be in Futurice.Prelude
import Control.Lens (makeWrapped)

-- | TODO: change to uuid
newtype Identifier a = Identifier Word64
    deriving (Eq, Ord, Show, Typeable, Generic)

newtype Name a = Name Text
    deriving (Eq, Ord, Show, Typeable, Generic)

-- | All checklist tasks are tied to the user
--
-- /TODO:/ add more fields? Is 'Employee' better name?
data User = User
    { _userId           :: Identifier User
    , _userFirstName    :: !Text
    , _userLastName     :: !Text
    , _userContractType :: !ContractType
    , _userLocation     :: !Location
    , _userConfirmed    :: !Bool
      -- ^ /TODO:/ What is this?
    , _userPhone        :: !Text
    , _userContactEmail :: !Text
      -- ^ /Note:/ This is non-work email!
    , _userStartingDay  :: !Day
    , _userSupervisor   :: !FUMLogin
    , _userTribe        :: !Text
      -- ^ /Note:/ ATM this is free form text.
    , _userInfo         :: !Text
      -- ^ Free text comments about the user.
    --
    , _userFUMLogin     :: !(Maybe FUMLogin)
    , _userHRNumber     :: !(Maybe Int) -- TODO: make a newtype for this
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

-- | Contract type affect what's need to be done.
data ContractType
    = ContractTypePermanent
    | ContractTypeExternal
    | ContractTypeFixedTerm
    | ContractTypePartTimer
    | ContractTypeSummerWorker
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)


-- | Tasks can be location specific.
data Location
    = LocHelsinki
    | LocTampere
    | LocBerlin
    | LocLondon
    | LocStockholm
    | LocMunich
    | LocOther
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

-- |
--
-- Maybe use 'FUM.UserName' ?
newtype FUMLogin = FUMLogin Text
    deriving (Eq, Ord, Show, Typeable, Generic)

-- | 'Task' describes a particular task needs to be done. For example /"add to fum"/ or /"order a laptop".
data Task = Task
    { _taskName'        :: !(Name Task)
      -- ^ Name also acts as a primary key
    , _taskCanBeDone    :: User -> Bool
      -- ^ Some tasks cannot be yet done, if some information is missing.
    , _taskDependencies :: !(Vector :$ Name Task)
      -- ^ Some tasks can be done only after some other tasks are done.
    , _taskCheck        :: User -> IO CheckCondition
      -- ^ Tasks can check themselves whether they are done. For example if 'userFUMLogin' is known,
      --   then when such user is seen in FUM, we can see that task is probably done.
    , _taskRole         :: !TaskRole
      -- ^ Tasks can be fullfilled by different roles.
    }
    deriving (Typeable, Generic)

data CheckCondition
    = CheckConditionSuccess
      -- ^ Everything is ok
    | CheckConditionMaybe
      -- ^ Non definitive answer, but doesn't prevent from completing task. E.g. long cache time might make user still invisible in FUM.
    | CheckConditionFailure
      -- ^ Definitively not ok, 'TaskItem' cannot be completed.
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)


-- | The role which is primarily interested in the task.
data TaskRole
    = TaskRoleIT
    | TaskRoleHR
    | TaskRoleSupervisor
    | TaskRoleOther
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)


-- | Checklist is collection of tasks. Used to group tasks together to create 'TaskItem's together.
--  Example lists are "new full-time employee in Helsinki"
data Checklist = Checklist
    { _checklistName  :: !Text -- tagged!
    , _checklistTasks :: !(Vector (Name Task, Maybe TaskAppliance))
    }
    deriving (Eq, Ord, Show, Typeable, Generic)


-- | 'TaskItem' is an instance of 'Task'. I.e. the actual task needed to be done.
data TaskItem = TaskItem
    { _taskItemId   :: !(Identifier TaskItem)
    , _taskItemUser :: !(Identifier User)
    , _taskItemTask :: !(Name Task)
    , _taskItemDone :: !Bool
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

-- | Task appliance, e.g. this task is /"only for Helsinki and permanent employees"/.
--
-- /TODO;/ define my. maybe depend on 'Contract' and 'Location'.
data TaskAppliance = TaskApplianceAll
    deriving (Eq, Ord, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeLenses ''User
makePrisms ''ContractType
makePrisms ''Location
makeWrapped ''FUMLogin
makeLenses ''Task
makePrisms ''CheckCondition
makePrisms ''TaskRole
makeLenses ''Checklist
makeLenses ''TaskItem

-------------------------------------------------------------------------------
-- Classy lenses
-------------------------------------------------------------------------------

class HasIdentifier entity ident | entity -> ident where
    identifier :: Lens' entity (Identifier ident)

instance HasIdentifier (Identifier e) e where
    identifier = id

instance HasIdentifier User User where
    identifier = userId

instance HasIdentifier TaskItem TaskItem where
    identifier = taskItemId

class HasTaskName entity where
    taskName :: Lens' entity (Name Task)

instance HasTaskName Task where
    taskName = taskName'

instance HasTaskName TaskItem where
    taskName = taskItemTask

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

deriveGeneric ''User
deriveGeneric ''ContractType
deriveGeneric ''Location
deriveGeneric ''FUMLogin
deriveGeneric ''Task
deriveGeneric ''CheckCondition
deriveGeneric ''TaskRole
deriveGeneric ''Checklist
deriveGeneric ''TaskItem
