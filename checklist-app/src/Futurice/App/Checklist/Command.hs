{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif

module Futurice.App.Checklist.Command (
    -- * Command
    Command (..),
    traverseCommand,
    CIT (..),
    applyCommand,
    transactCommand,
    -- * Edits
    TaskEdit (..),
    applyTaskEdit,
    EmployeeEdit (..),
    fromEmployeeEdit,
    applyEmployeeEdit,
    ) where

import Prelude ()
import Futurice.Prelude
import Algebra.Lattice            (top)
import Control.Lens               (iforOf_, non, use)
import Control.Monad.State.Strict (execState)
import Data.Swagger               (NamedSchema (..))
import Data.Singletons.Bool
import Data.Type.Equality
import Futurice.Aeson
       (FromJSONField1, fromJSONField1, object, withObject, (.!=), (.:), (.:?),
       (.=))
import Futurice.Generics
import Futurice.IsMaybe

import qualified Control.Lens                         as Lens
import qualified Data.Aeson.Compat                    as Aeson
import qualified Database.PostgreSQL.Simple           as Postgres
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified FUM
import qualified Generics.SOP                         as SOP

import Futurice.App.Checklist.Types

-------------------------------------------------------------------------------
-- Task edit
-------------------------------------------------------------------------------

data TaskEdit f = TaskEdit
    { teName    :: !(f :$ Name Task)
    , teRole    :: !(f TaskRole)
    , tePrereqs :: !(f :$ Set :$ Identifier Task)
    }

deriveGeneric ''TaskEdit

applyTaskEdit :: TaskEdit Maybe -> Task -> Task
applyTaskEdit te
    = maybe id (Lens.set taskName) (teName te)
    . maybe id (Lens.set taskRole) (teRole te)
    . maybe id (Lens.set taskPrereqs) (tePrereqs te)

instance Eq1 f => Eq (TaskEdit f) where
    TaskEdit n r pr == TaskEdit n' r' pr' =
        eq1 n n' && eq1 r r' && eq1 pr pr'

instance Show1 f => Show (TaskEdit f) where
    showsPrec d (TaskEdit n r pr) = showsTernaryWith
        showsPrec1 showsPrec1 showsPrec1
        "TaskEdit" d n r pr

type TaskEditTypes = '[Name Task, TaskRole, Set :$ Identifier Task]

instance SOP.All (SOP.Compose Arbitrary f) TaskEditTypes
    => Arbitrary (TaskEdit f)
  where
    arbitrary = sopArbitrary
    shrink = sopShrink

instance
    ( SOP.All (SOP.Compose ToJSON f) TaskEditTypes
    , SOP.All (SOP.Compose IsMaybe f) TaskEditTypes
    )
    => ToJSON (TaskEdit f)
  where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance
    ( SOP.All (SOP.Compose FromJSON f) TaskEditTypes
    , SBoolI (f == Maybe), Applicative f
    )
    => FromJSON (TaskEdit f)
  where
    -- We don't use sopParseJSON because of special treatment of prereqs
    -- in not Maybe case (i.e. Identity)
    parseJSON = withObject "TaskEdit" $ \obj ->
        case sboolEqRefl :: Maybe (f :~: Maybe) of
            Just Refl -> TaskEdit
                <$> obj .:? "name"
                <*> obj .:? "role"
                <*> obj .:? "prereqs"
            Nothing -> TaskEdit
                <$> obj .: "name"
                <*> obj .: "role"
                <*> obj .:? "prereqs" .!= pure mempty

-------------------------------------------------------------------------------
-- Employee edit
-------------------------------------------------------------------------------

data EmployeeEdit f = EmployeeEdit
    { eeFirstName    :: !(f Text)
    , eeLastName     :: !(f Text)
    , eeContractType :: !(f ContractType)
    , eeLocation     :: !(f Location)
    , eeConfirmed    :: !(f Bool)
    , eeStartingDay  :: !(f Day)
    , eeSupervisor   :: !(f FUM.UserName)
    , eeTribe        :: !(f Text)
    , eeInfo         :: !(f Text)
    -- this fields are optional
    , eePhone        :: !(Maybe Text)
    , eeContactEmail :: !(Maybe Text)
    , eeFumLogin     :: !(Maybe FUM.UserName)
    , eeHrNumber     :: !(Maybe Int)
    }

deriveGeneric ''EmployeeEdit

fromEmployeeEdit
    :: Identifier Employee
    -> Identifier Checklist
    -> EmployeeEdit Identity
    -> Employee
fromEmployeeEdit eid cid EmployeeEdit {..} = Employee
    { _employeeId           = eid
    , _employeeChecklist    = cid
    , _employeeFirstName    = runIdentity eeFirstName
    , _employeeLastName     = runIdentity eeLastName
    , _employeeContractType = runIdentity eeContractType
    , _employeeLocation     = runIdentity eeLocation
    , _employeeConfirmed    = runIdentity eeConfirmed
    , _employeeStartingDay  = runIdentity eeStartingDay
    , _employeeSupervisor   = runIdentity eeSupervisor
    , _employeeTribe        = runIdentity eeTribe
    , _employeeInfo         = runIdentity eeInfo
    , _employeePhone        = eePhone
    , _employeeContactEmail = eeContactEmail
    , _employeeFUMLogin     = eeFumLogin
    , _employeeHRNumber     = eeHrNumber
    }

applyEmployeeEdit :: EmployeeEdit Maybe -> Employee -> Employee
applyEmployeeEdit ee
    = maybe id (Lens.set employeeFirstName) (eeFirstName ee)
    . maybe id (Lens.set employeeLastName) (eeLastName ee)
    . maybe id (Lens.set employeeContractType) (eeContractType ee)
    . maybe id (Lens.set employeeLocation) (eeLocation ee)
    . maybe id (Lens.set employeeConfirmed) (eeConfirmed ee)
    . maybe id (Lens.set employeeStartingDay) (eeStartingDay ee)
    . maybe id (Lens.set employeeSupervisor) (eeSupervisor ee)
    . maybe id (Lens.set employeeTribe) (eeTribe ee)
    . maybe id (Lens.set employeeInfo) (eeInfo ee)
    . Lens.over employeePhone (eePhone ee <|>)
    . Lens.over employeeContactEmail (eeContactEmail ee <|>)
    . Lens.over employeeFUMLogin (eeFumLogin ee <|>)
    . Lens.over employeeHRNumber (eeHrNumber ee <|>)

instance Eq1 f => Eq (EmployeeEdit f) where
    a == b
        = eq1 (eeFirstName a) (eeFirstName b)
        && eq1 (eeLastName a) (eeLastName b)
        && eq1 (eeContractType a) (eeContractType b)

instance Show1 f => Show (EmployeeEdit f) where
    showsPrec d EmployeeEdit {..} = showParen (d > 10)
        $ showString "EmployeeEdit"
        . showChar ' ' . showsPrec1 11 eeFirstName
        . showChar ' ' . showsPrec1 11 eeLastName

type EmployeeEditFieldTypes =
    '[Text, ContractType, Location, Bool, FUM.UserName, Int, Day]

instance SOP.All (SOP.Compose Arbitrary f) EmployeeEditFieldTypes
    => Arbitrary (EmployeeEdit f)
  where
    arbitrary = sopArbitrary
    shrink = sopShrink

instance
    ( SOP.All (SOP.Compose ToJSON f) EmployeeEditFieldTypes
    , SOP.All (SOP.Compose IsMaybe f) EmployeeEditFieldTypes
    )
    => ToJSON (EmployeeEdit f)
  where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance
    ( SOP.All (SOP.Compose FromJSON f) EmployeeEditFieldTypes
    , SOP.All (SOP.Compose IsMaybe f) EmployeeEditFieldTypes
    )
    => FromJSON (EmployeeEdit f)
  where
    parseJSON = sopParseJSON

-------------------------------------------------------------------------------
-- TaskAddition
-------------------------------------------------------------------------------

data TaskAddition = TaskAddition (Identifier Checklist) TaskAppliance
  deriving (Eq, Show)

deriveGeneric ''TaskAddition

instance Arbitrary TaskAddition where
    arbitrary = sopArbitrary
    shrink = sopShrink

instance ToJSON TaskAddition where
    toJSON (TaskAddition cid app) = object [ "cid" .= cid, "app" .= app ]
    toEncoding (TaskAddition cid app) = Aeson.pairs ( "cid" .= cid <> "app" .= app )

instance FromJSON TaskAddition where
    parseJSON = withObject "TaskAddition" $ \obj -> TaskAddition
        <$> obj .: "cid"
        <*> obj .:? "app" .!= top

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

-- | Command as in CQRS
data Command f
    = CmdCreateChecklist (f :$ Identifier Checklist) (Name Checklist)
    | CmdRenameChecklist (Identifier Checklist) (Name Checklist)
    | CmdCreateTask (f :$ Identifier Task) (TaskEdit Identity) [TaskAddition]
    | CmdEditTask (Identifier Task) (TaskEdit Maybe)
    | CmdAddTask (Identifier Checklist) (Identifier Task) TaskAppliance
    | CmdRemoveTask (Identifier Checklist) (Identifier Task)
    | CmdCreateEmployee (f :$ Identifier Employee) (Identifier Checklist) (EmployeeEdit Identity)
    | CmdEditEmployee (Identifier Employee) (EmployeeEdit Maybe)
    | CmdTaskItemToggle (Identifier Employee) (Identifier Task) TaskItem

-- CmdEditTaskAppliance
-- CmdArchiveEmployee

deriveGeneric ''Command

data CIT a where
    CITTask      :: CIT Task
    CITEmployee  :: CIT Employee
    CITChecklist :: CIT Checklist

traverseCommand
    :: Applicative m
    => (forall x. CIT x -> f (Identifier x) -> m (g (Identifier x)))
    -> Command f
    -> m (Command g)
traverseCommand  f (CmdCreateChecklist i n) =
    CmdCreateChecklist <$> f CITChecklist i <*> pure n
traverseCommand _f (CmdRenameChecklist i n) =
    pure $ CmdRenameChecklist i n
traverseCommand f (CmdCreateTask i e ls ) =
    CmdCreateTask <$> f CITTask i <*> pure e <*> pure ls
traverseCommand _f (CmdEditTask i e) =
    pure $ CmdEditTask i e
traverseCommand _f (CmdAddTask c t a) =
    pure $ CmdAddTask c t a
traverseCommand _f (CmdRemoveTask c t) =
    pure $ CmdRemoveTask c t
traverseCommand f (CmdCreateEmployee e c x) =
    CmdCreateEmployee <$> f CITEmployee e <*> pure c <*> pure x
traverseCommand _f (CmdEditEmployee e x) =
    pure $ CmdEditEmployee e x
traverseCommand _ (CmdTaskItemToggle e t x) =
    pure $ CmdTaskItemToggle e t x

-- = operators are the same as ~ lens operators, but modify the state of MonadState.
--
-- todo: in error monad, if e.g. identifier don't exist
applyCommand :: Command Identity -> World -> World
applyCommand cmd world = flip execState world $ case cmd of
    CmdCreateChecklist (Identity cid) n ->
        worldLists . at cid ?= Checklist cid n mempty

    CmdCreateTask (Identity tid) (TaskEdit (Identity n) (Identity role) (Identity pr)) ls -> do
        worldTasks . at tid ?= Task tid n pr role
        for_ ls $ \(TaskAddition cid app) ->
            worldLists . ix cid . checklistTasks . at tid ?= app

    CmdAddTask cid tid app -> do
        worldLists . ix cid . checklistTasks . at tid ?= app
        es <- toList <$> use worldEmployees
        for_ es $ \e -> do
            let eid = e ^. identifier
            when (e ^. employeeChecklist == cid && employeeTaskApplies e app) $ do
                worldTaskItems . at eid . non mempty . at tid %= Just . fromMaybe TaskItemTodo

    CmdRemoveTask cid tid ->
        worldLists . ix cid . checklistTasks . at tid Lens..= Nothing

    CmdRenameChecklist cid n ->
         worldLists . ix cid . checklistName Lens..= n

    CmdEditTask tid te ->
        worldTasks . ix tid %= applyTaskEdit te

    CmdCreateEmployee (Identity eid) cid x -> do
        -- create user
        let e = fromEmployeeEdit eid cid x
        worldEmployees . at eid ?= e
        -- add initial tasks
        iforOf_ (worldLists . ix cid . checklistTasks . ifolded) world $ \tid app ->
            when (employeeTaskApplies e app) $
                worldTaskItems . at eid . non mempty . at tid ?= TaskItemTodo

    CmdEditEmployee eid x -> do
        worldEmployees . ix eid %= applyEmployeeEdit x

    CmdTaskItemToggle eid tid d ->
        worldTaskItems . ix eid . ix tid Lens..= d

transactCommand
    :: (MonadLog m, MonadIO m)
    => Postgres.Connection -> FUM.UserName -> Command Identity -> m ()
transactCommand conn ssoUser cmd = do
    logInfo "transactCommand" cmd
    _ <- liftIO $ Postgres.execute conn
        "INSERT INTO checklist2.commands (username, cmddata) VALUES (?, ?)"
        (ssoUser, cmd)
    pure ()

instance Eq1 f => Eq (Command f) where
    CmdCreateChecklist cid n == CmdCreateChecklist cid' n'
        = eq1 cid cid' && n == n'
    CmdRenameChecklist cid n == CmdRenameChecklist cid' n'
        = cid == cid' && n == n'
    CmdCreateTask tid te ls == CmdCreateTask tid' te' ls'
        = eq1 tid tid' && te == te' && ls == ls'
    CmdEditTask tid te == CmdEditTask tid' te'
        = tid == tid' && te == te'
    CmdAddTask cid tid app == CmdAddTask cid' tid' app'
        = cid == cid' && tid == tid' && app == app'
    CmdRemoveTask cid tid == CmdRemoveTask cid' tid'
        = cid == cid' && tid == tid'
    CmdCreateEmployee eid cid x == CmdCreateEmployee eid' cid' x'
        = eq1 eid eid' && cid == cid' && x == x'
    CmdEditEmployee eid x == CmdEditEmployee eid' x'
        = eid == eid' && x == x'
    CmdTaskItemToggle eid tid d == CmdTaskItemToggle eid' tid' d'
        = eid == eid' && tid == tid' && d == d'

    -- Otherwise false
    _ == _ = False

instance Show1 f => Show (Command f) where
    showsPrec d (CmdCreateChecklist i n) = showsBinaryWith
        showsPrec1 showsPrec
        "CmdCreateChecklist" d i n
    showsPrec d (CmdRenameChecklist i n) = showsBinaryWith
        showsPrec showsPrec
        "CmdRenameChecklist" d i n
    showsPrec d (CmdCreateTask i te ls) = showsTernaryWith
        showsPrec1 showsPrec showsPrec
        "CmdCreateTask" d i te ls
    showsPrec d (CmdEditTask i te) = showsBinaryWith
        showsPrec showsPrec
        "CmdEditTask" d i te
    showsPrec d (CmdAddTask c t a) = showsTernaryWith
        showsPrec showsPrec showsPrec
        "CmdAddTask" d c t a
    showsPrec d (CmdRemoveTask c t) = showsBinaryWith
        showsPrec showsPrec
        "CmdRemoveTask" d c t
    showsPrec d (CmdCreateEmployee e c x) = showsTernaryWith
        showsPrec1 showsPrec showsPrec
        "CmdCreateEmployee" d e c x
    showsPrec d (CmdEditEmployee e x) = showsBinaryWith
        showsPrec showsPrec
        "CmdEditEmployee" d e x
    showsPrec d (CmdTaskItemToggle e t done) = showsTernaryWith
        showsPrec showsPrec showsPrec
        "CmdTaskItemToggle" d e t done

instance SOP.All (SOP.Compose Arbitrary f) '[Identifier Checklist, Identifier Task, Identifier Employee]
    => Arbitrary (Command f)
  where
    arbitrary = sopArbitrary
    shrink = sopShrink

-- | This and 'ParseJSON' instance is written by hand, as 'sopToJSON' and friends
-- work with records only, and we want field names!
instance SOP.All (SOP.Compose ToJSON f) '[Identifier Checklist, Identifier Task, Identifier Employee]
    => ToJSON (Command f)
  where
    toJSON (CmdCreateChecklist cid n) = object
        [ "cmd"  .= ("create-checklist" :: Text)
        , "cid"  .= cid
        , "name" .= n
        ]
    toJSON (CmdRenameChecklist cid n) = object
        [ "cmd"  .= ("rename-checklist" :: Text)
        , "cid"  .= cid
        , "name" .= n
        ]
    toJSON (CmdCreateTask tid te lists) = object
        [ "cmd"  .= ("create-task" :: Text)
        , "tid"  .= tid
        , "edit" .= te
        , "lists" .= lists
        ]
    toJSON (CmdEditTask tid te) = object
        [ "cmd"  .= ("edit-task" :: Text)
        , "tid"  .= tid
        , "edit" .= te
        ]
    toJSON (CmdAddTask cid tid app) = object
        [ "cmd"       .= ("add-task" :: Text)
        , "cid"       .= cid
        , "tid"       .= tid
        , "appliance" .= app
        ]
    toJSON (CmdRemoveTask cid tid) = object
        [ "cmd"       .= ("remove-task" :: Text)
        , "cid"       .= cid
        , "tid"       .= tid
        ]
    toJSON (CmdCreateEmployee eid cid x) = object
        [ "cmd"  .= ("create-employee" :: Text)
        , "eid"  .= eid
        , "cid"  .= cid
        , "edit" .= x
        ]
    toJSON (CmdEditEmployee eid x) = object
        [ "cmd"  .= ("edit-employee" :: Text)
        , "eid"  .= eid
        , "edit" .= x
        ]
    toJSON (CmdTaskItemToggle eid tid done) = object
        [ "cmd"  .= ("task-item-toggle" :: Text)
        , "eid"  .= eid
        , "tid"  .= tid
        , "done" .= done
        ]

    -- toEncoding

instance FromJSONField1 f => FromJSON (Command f)
  where
    parseJSON = withObject "Command" $ \obj -> do
        cmd <- obj .: "cmd" :: Aeson.Parser Text
        case cmd of
            "create-checklist" -> CmdCreateChecklist
                <$> fromJSONField1 obj "cid"
                <*> obj .: "name"
            "rename-checklist" -> CmdRenameChecklist
                <$> obj .: "cid"
                <*> obj .: "name"
            "create-task" -> CmdCreateTask
                <$> fromJSONField1 obj "tid"
                <*> obj .: "edit"
                <*> obj .:? "lists" .!= []
            "edit-task" -> CmdEditTask
                <$> obj .: "tid"
                <*> obj .: "edit"
            "add-task" -> CmdAddTask
                <$> obj .: "cid"
                <*> obj .: "tid"
                <*> obj .:? "appliance" .!= top
            "remove-task" -> CmdRemoveTask
                <$> obj .: "cid"
                <*> obj .: "tid"
            "create-employee" -> CmdCreateEmployee
                <$> fromJSONField1 obj "eid"
                <*> obj .: "cid"
                <*> obj .: "edit"
            "edit-employee" -> CmdEditEmployee
                <$> obj .: "eid"
                <*> obj .: "edit"
            "task-item-toggle" -> CmdTaskItemToggle
                <$> obj .: "eid"
                <*> obj .: "tid"
                <*> obj .: "done"

            _ -> fail $ "Invalid Command tag " ++ cmd ^. unpacked

-- TODO
instance ToSchema (Command p) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Command") mempty

instance SOP.All (SOP.Compose ToJSON f) '[Identifier Checklist, Identifier Task, Identifier Employee]
    => Postgres.ToField (Command f)
  where
    toField = Postgres.toField . Aeson.encode

instance FromJSONField1 f => Postgres.FromField (Command f) where
    fromField f mdata = do
        bs <- Postgres.fromField f mdata
        case Aeson.eitherDecode bs of
            Right x  -> pure x
            Left err -> Postgres.conversionError (Aeson.AesonException err)
