{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Basic types
module Futurice.App.Checklist.Types.Basic where

import Control.Lens      (Getter, Prism', prism')
import Data.Aeson.Compat (Value (String))
import Data.Swagger
       (SwaggerType (SwaggerString), ToParamSchema (..), enum_, type_)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Servant           (FromHttpApiData (..), ToHttpApiData (..))
import Futurice.Arbitrary (arbitraryNoun, arbitraryAdjective, arbitraryVerb)

import Futurice.App.Checklist.Types.Identifier

import qualified Data.Map        as Map
import qualified Data.Text       as T
import qualified Test.QuickCheck as QC

newtype Name a = Name Text
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | All checklist tasks are tied to the employee
--
-- /TODO:/ add more fields? Is 'Employee' better name?
data Employee = Employee
    { _employeeId           :: Identifier Employee
    , _employeeChecklist    :: Identifier Checklist
    , _employeeFirstName    :: !Text
    , _employeeLastName     :: !Text
    , _employeeContractType :: !ContractType
    , _employeeLocation     :: !Location
    , _employeeConfirmed    :: !Bool
      -- ^ /TODO:/ What is this?
    , _employeePhone        :: !Text
    , _employeeContactEmail :: !Text
      -- ^ /Note:/ This is non-work email!
    , _employeeStartingDay  :: !Day
    , _employeeSupervisor   :: !FUMLogin
    , _employeeTribe        :: !Text
      -- ^ /Note:/ ATM this is free form text.
    , _employeeInfo         :: !Text
      -- ^ Free text comments about the employee.
    -- Data filled up later:
    , _employeeFUMLogin     :: !(Maybe FUMLogin)
    , _employeeHRNumber     :: !(Maybe Int) -- TODO: make a newtype for this
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
-- Maybe use 'FUM.EmployeeName' ?
newtype FUMLogin = FUMLogin Text
    deriving (Eq, Ord, Show, Typeable, Generic)

-- | 'Task' describes a particular task needs to be done. For example /"add to fum"/ or /"order a laptop".
data Task = Task
    { _taskId           :: !(Identifier Task)
    , _taskName         :: !(Name Task)
      -- ^ Display name
    , _taskCanBeDone    :: Employee -> Bool
      -- ^ Some tasks cannot be yet done, if some information is missing.
    , _taskDependencies :: !(Set :$ Identifier Task)
      -- ^ Some tasks can be done only after some other tasks are done.
    , _taskCheck        :: Employee -> IO CheckResult
      -- ^ Tasks can check themselves whether they are done. For example if 'employeeFUMLogin' is known,
      --   then when such employee is seen in FUM, we can see that task is probably done.
    , _taskRole         :: !TaskRole
      -- ^ Tasks can be fullfilled by different roles.
    }
  deriving (Typeable, Generic)

-- TODO: Show Task debugging instance

-- |
data CheckResult
    = CheckResultSuccess
      -- ^ Everything is ok
    | CheckResultMaybe
      -- ^ Non definitive answer, but doesn't prevent from completing task. E.g. long cache time might make employee still invisible in FUM.
    | CheckResultFailure
      -- ^ Definitively not ok, task cannot be completed.
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)


-- | The role which is primarily interested in the task.
data TaskRole
    = TaskRoleIT
    | TaskRoleHR
    | TaskRoleSupervisor
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)


-- | Checklist is collection of tasks. Used to group tasks together to create task instances together.
--  Example lists are "new full-time employee in Helsinki"
data Checklist = Checklist
    { _checklistId    :: !(Identifier Checklist)
    , _checklistName  :: !(Name Checklist)
    , _checklistTasks :: !(Map (Identifier Task) TaskAppliance)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

data TaskItemDone
    = TaskItemDone
    | TaskItemTodo
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | Task appliance, e.g. this task is /"only for Helsinki and permanent employees"/.
--
-- /TODO;/ define my. maybe depend on 'Contract' and 'Location'.
data TaskAppliance = TaskApplianceAll
  deriving (Eq, Ord, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeWrapped ''Name
makeLenses ''Employee
makePrisms ''ContractType
makePrisms ''Location
makeWrapped ''FUMLogin
makeLenses ''Task
makePrisms ''CheckResult
makePrisms ''TaskRole
makeLenses ''Checklist
makePrisms ''TaskItemDone

-------------------------------------------------------------------------------
-- HasIdentifier instances
-------------------------------------------------------------------------------

instance HasIdentifier Employee Employee where
    identifier = employeeId

instance HasIdentifier Task Task where
    identifier = taskId

instance HasIdentifier Checklist Checklist where
    identifier = checklistId

-------------------------------------------------------------------------------
-- Some arbitraries
-------------------------------------------------------------------------------

class    HasName a         where name :: Getter a (Name a)
instance HasName Task      where name = taskName
instance HasName Checklist where name = checklistName

class ArbitraryName a where
    arbitraryName :: QC.Gen (Name a)

instance ArbitraryName Task where
    arbitraryName = (\a b -> Name $ T.toTitle a <> " " <> b)
        <$> arbitraryVerb
        <*> arbitraryNoun

instance ArbitraryName Checklist where
    arbitraryName = (\a b -> Name $ T.toTitle a <> " " <> b)
        <$> arbitraryAdjective
        <*> arbitraryNoun

instance ArbitraryName a => Arbitrary (Name a) where
    arbitrary = arbitraryName

instance Arbitrary FUMLogin where
    arbitrary = FUMLogin <$> gen
      where
        gen        = mk <$> g <*> g <*> g <*> g
        mk a b c d = [a,b,c,d] ^. packed
        g          = QC.elements ['a'..'z']

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

deriveGeneric ''Employee
deriveGeneric ''ContractType
deriveGeneric ''Location
deriveGeneric ''FUMLogin
deriveGeneric ''Task
deriveGeneric ''CheckResult
deriveGeneric ''TaskRole
deriveGeneric ''Checklist
deriveGeneric ''TaskItemDone

instance Arbitrary Employee where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Arbitrary TaskItemDone where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Arbitrary Checklist where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Arbitrary Location where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Arbitrary ContractType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Arbitrary TaskRole where
    arbitrary = sopArbitrary
    shrink    = sopShrink

-- | Returns always 'TaskApplianceAll'
instance Arbitrary TaskAppliance where
    arbitrary = pure TaskApplianceAll
    shrink    = const []

-- | /TODO/: no shrink
instance Arbitrary Task where
    arbitrary = Task
        <$> arbitrary
        <*> arbitrary
        <*> pure (const True)
        <*> arbitrary
        <*> pure (const (pure CheckResultMaybe))
        <*> arbitrary

    shrink    = const []

-------------------------------------------------------------------------------
-- Location servant schema
-------------------------------------------------------------------------------

locationToText :: Location -> Text
locationToText LocHelsinki  = "Helsinki"
locationToText LocTampere   = "Tampere"
locationToText LocBerlin    = "Berlin"
locationToText LocLondon    = "London"
locationToText LocStockholm = "Stockholm"
locationToText LocMunich    = "Munich"
locationToText LocOther     = "Other"

locationFromText :: Text -> Maybe Location
locationFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ locationToText x, x)) [minBound .. maxBound]

_Location :: Prism' Text Location
_Location = prism' locationToText locationFromText

instance ToParamSchema Location where
    toParamSchema _ = mempty
          & type_ .~ SwaggerString
          & enum_ ?~ (String . locationToText <$> [minBound .. maxBound])

instance FromHttpApiData Location where
    parseUrlPiece t =
        maybe (Left $ "invalid location " <> t) Right $ t ^? _Location

instance ToHttpApiData Location where
    toUrlPiece = locationToText

-------------------------------------------------------------------------------
-- Task role
-------------------------------------------------------------------------------

roleToText :: TaskRole -> Text
roleToText TaskRoleIT         = "IT"
roleToText TaskRoleHR         = "HR"
roleToText TaskRoleSupervisor = "Supervisor"

roleFromText :: Text -> Maybe TaskRole
roleFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ roleToText x, x)) [minBound .. maxBound]

_TaskRole :: Prism' Text TaskRole
_TaskRole = prism' roleToText roleFromText

instance ToParamSchema TaskRole where
    toParamSchema _ = mempty
          & type_ .~ SwaggerString
          & enum_ ?~ (String . roleToText <$> [minBound .. maxBound])

instance FromHttpApiData TaskRole where
    parseUrlPiece t =
        maybe (Left $ "invalid location " <> t) Right $ t ^? _TaskRole

instance ToHttpApiData TaskRole where
    toUrlPiece = roleToText
