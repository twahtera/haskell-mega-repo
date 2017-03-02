{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
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
import Data.Char                  (isUpper, toLower)
import Data.List                  (intercalate)
import Data.List.CommonPrefix     (CommonPrefix (..), getCommonPrefix)
import Data.List.Split            (keepDelimsL, split, whenElt)
import Data.Singletons.Bool
import Data.Swagger               (NamedSchema (..))
import Data.Type.Equality
import Futurice.Aeson
       (FromJSONField1, fromJSONField1, object, withBool, withObject, (.!=),
       (.:), (.:?), (.=))
import Futurice.Generics
import Futurice.IsMaybe

import qualified Control.Lens                         as Lens
import qualified Data.Aeson.Compat                    as Aeson
import qualified Database.PostgreSQL.Simple           as Postgres
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified FUM
import qualified Generics.SOP                         as SOP
import qualified Generics.SOP.Lens                    as SOPL

import Futurice.App.Checklist.Types

-------------------------------------------------------------------------------
-- Task edit
-------------------------------------------------------------------------------

data TaskEdit f = TaskEdit
    { teName    :: !(f :$ Name Task)
    , teInfo    :: !(f :$ Text)
    , teRole    :: !(f TaskRole)
    , tePrereqs :: !(f :$ Set :$ Identifier Task)
    , teComment :: !(f :$ Bool)
    }

deriveGeneric ''TaskEdit

applyTaskEdit :: TaskEdit Maybe -> Task -> Task
applyTaskEdit te
    = maybe id (Lens.set taskName) (teName te)
    . maybe id (Lens.set taskInfo) (teInfo te)
    . maybe id (Lens.set taskRole) (teRole te)
    . maybe id (Lens.set taskPrereqs) (tePrereqs te)
    . maybe id (Lens.set taskComment) (teComment te)

type TaskEditTypes = '[Name Task, TaskRole, Set :$ Identifier Task, Text, Bool]

deriving instance SOP.All (SOP.Compose Eq f) TaskEditTypes => Eq (TaskEdit f)
deriving instance SOP.All (SOP.Compose Show f) TaskEditTypes => Show (TaskEdit f)

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
                <*> obj .:? "info"
                <*> obj .:? "role"
                <*> obj .:? "prereqs"
                <*> obj .:? "comment"
            Nothing -> TaskEdit
                <$> obj .: "name"
                <*> obj .:? "info" .!= pure mempty
                <*> obj .: "role"
                <*> obj .:? "prereqs" .!= pure mempty
                <*> obj .:? "comment" .!= pure False

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

type EmployeeEditTypes =
    '[Text, ContractType, Location, Bool, FUM.UserName, Int, Day]

deriving instance SOP.All (SOP.Compose Eq f) EmployeeEditTypes => Eq (EmployeeEdit f)
deriving instance SOP.All (SOP.Compose Show f) EmployeeEditTypes => Show (EmployeeEdit f)

instance SOP.All (SOP.Compose Arbitrary f) EmployeeEditTypes
    => Arbitrary (EmployeeEdit f)
  where
    arbitrary = sopArbitrary
    shrink = sopShrink

instance
    ( SOP.All (SOP.Compose ToJSON f) EmployeeEditTypes
    , SOP.All (SOP.Compose IsMaybe f) EmployeeEditTypes
    )
    => ToJSON (EmployeeEdit f)
  where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance
    ( SOP.All (SOP.Compose FromJSON f) EmployeeEditTypes
    , SOP.All (SOP.Compose IsMaybe f) EmployeeEditTypes
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
    | CmdArchiveEmployee (Identifier Employee) ArchiveOrRemove
    | CmdTaskEditComment (Identifier Employee) (Identifier Task) TaskComment

data ArchiveOrRemove = Archive | Remove deriving (Eq, Show)

deriveGeneric ''Command
deriveGeneric ''ArchiveOrRemove

instance ToJSON ArchiveOrRemove where
    toJSON Archive = toJSON False
    toJSON Remove  = toJSON True
instance FromJSON ArchiveOrRemove where
    parseJSON = withBool "ArchiveOrRemove" $ \b ->
        pure $ if b then Remove else Archive
instance Arbitrary ArchiveOrRemove where
    arbitrary = sopArbitrary



-- | Command identifier tag.
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
traverseCommand _ (CmdArchiveEmployee cid b) =
    pure $ CmdArchiveEmployee cid b
traverseCommand _ (CmdTaskEditComment eid tid c) =
    pure $ CmdTaskEditComment eid tid c

-- = operators are the same as ~ lens operators, but modify the state of MonadState.
--
-- todo: in error monad, if e.g. identifier don't exist
applyCommand :: UTCTime -> FUM.UserName -> Command Identity -> World -> World
applyCommand now ssoUser cmd world = flip execState world $ case cmd of
    CmdCreateChecklist (Identity cid) n ->
        worldLists . at cid ?= Checklist cid n mempty

    CmdCreateTask (Identity tid) (TaskEdit (Identity n) (Identity i) (Identity role) (Identity pr) (Identity comment)) ls -> do
        worldTasks . at tid ?= Task tid n i pr role comment
        for_ ls $ \(TaskAddition cid app) -> addTask cid tid app

    CmdAddTask cid tid app -> addTask cid tid app

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
                worldTaskItems . at eid . non mempty . at tid ?= annTaskItemTodo

    CmdEditEmployee eid x -> do
        worldEmployees . ix eid %= applyEmployeeEdit x

    CmdTaskItemToggle eid tid d -> do
        let d' = case d of
                TaskItemTodo -> annTaskItemTodo
                TaskItemDone -> AnnTaskItemDone "" ssoUser now
        worldTaskItems . ix eid . ix tid Lens..= d'

    CmdTaskEditComment eid tid (TaskComment c) -> do
        worldTaskItems . ix eid . ix tid . annTaskItemComment Lens..= c

    -- TODO: differentiate between archiving and deleting. Now we delete.
    CmdArchiveEmployee eid _ -> do
        worldTaskItems . at eid Lens..= Nothing
        worldEmployees . at eid Lens..= Nothing

  where
    -- tasks are added with both explicit CmdAddTask and during CmdCreateTask
    addTask cid tid app = do
        worldLists . ix cid . checklistTasks . at tid ?= app
        es <- toList <$> use worldEmployees
        for_ es $ \e -> do
            let eid = e ^. identifier
            when (e ^. employeeChecklist == cid && employeeTaskApplies e app) $ do
                worldTaskItems . at eid . non mempty . at tid %= Just . fromMaybe annTaskItemTodo

transactCommand
    :: (MonadLog m, MonadIO m)
    => Postgres.Connection -> FUM.UserName -> Command Identity -> m ()
transactCommand conn ssoUser cmd = do
    logInfo "transactCommand" cmd
    _ <- liftIO $ Postgres.execute conn
        "INSERT INTO checklist2.commands (username, cmddata) VALUES (?, ?)"
        (ssoUser, cmd)
    pure ()

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

type CommandIdentifiers =
    '[Identifier Checklist, Identifier Task, Identifier Employee]

deriving instance SOP.All (SOP.Compose Eq f) CommandIdentifiers => Eq (Command f)
deriving instance SOP.All (SOP.Compose Show f) CommandIdentifiers => Show (Command f)

instance SOP.All (SOP.Compose Arbitrary f) CommandIdentifiers => Arbitrary (Command f)
  where
    arbitrary = sopArbitrary
    shrink = sopShrink

-------------------------------------------------------------------------------
-- ToJSON
-------------------------------------------------------------------------------

instance SOP.All (SOP.Compose ToJSONField f) CommandIdentifiers => ToJSON (Command f) where
    toJSON = sumSopToJSON

-- | Class giving each type a key name
class ToJSON a => ToJSONField a where
    toJSONField :: a -> Maybe AesonPair

instance ToJSONField a => ToJSONField (Identity a) where
    toJSONField = toJSONField . runIdentity

instance ToJSONField [TaskAddition] where
    toJSONField [] = Nothing
    toJSONField xs = Just $ "lists" .= xs

instance ToJSON (TaskEdit f) => ToJSONField (TaskEdit f) where
    toJSONField = Just . ("edit" .=)
instance ToJSON (EmployeeEdit f) => ToJSONField (EmployeeEdit f) where
    toJSONField = Just . ("edit" .=)

instance ToJSONField (Identifier Task)      where toJSONField = Just . ("tid" .=)
instance ToJSONField (Identifier Employee)  where toJSONField = Just . ("eid" .=)
instance ToJSONField (Identifier Checklist) where toJSONField = Just . ("cid" .=)
instance ToJSONField (Name Checklist)       where toJSONField = Just . ("name" .=)
instance ToJSONField TaskComment            where toJSONField = Just . ("comment" .=)
instance ToJSONField ArchiveOrRemove        where toJSONField = Just . ("delete" .=)
instance ToJSONField TaskItem               where toJSONField = Just . ("done" .=)
instance ToJSONField TaskAppliance          where toJSONField = Just . ("appliance" .=)

sumSopToJSON
    :: forall a.
       (SOP.Generic a, SOP.HasDatatypeInfo a, SOP.All2 ToJSONField (SOP.Code a))
    => a
    -> Aeson.Value
sumSopToJSON
    = SOP.hcollapse
    . SOP.hcliftA2 pAllToJSONField f cinfos
    . SOP.unSOP . SOP.from
  where

    f :: SOP.All ToJSONField xs
      => SOP.ConstructorInfo xs -> NP I xs -> K Value xs
    f cinfo xs = K $ object $ ("cmd" .= cmd) :
        catMaybes (SOP.hcollapse . SOP.hcmap pToJSONField (K . toJSONField . unI) $ xs)
      where
        cmd = dashify $ drop (length prefix) $ cinfo ^. SOPL.constructorName

    pToJSONField :: Proxy ToJSONField
    pToJSONField = Proxy

    pAllToJSONField :: Proxy (SOP.All ToJSONField)
    pAllToJSONField = Proxy

    cinfos :: NP SOP.ConstructorInfo (SOP.Code a)
    cinfos = SOP.datatypeInfo (Proxy :: Proxy a) ^. SOPL.constructorInfo

    prefix :: String
    prefix = case cnames of
        []     -> []
        [_]    -> []
        (x:xs) -> getCommonPrefix $ foldMap1 CommonPrefix $ x :| xs
      where
        cnames = SOP.hcollapse $ SOP.hmap (K . view SOPL.constructorName) cinfos

    dashify :: String -> String
    dashify
        = map toLower
        . intercalate "-"
        . filter (not . null)
        . split (keepDelimsL $ whenElt isUpper)

    -- TODO:toEncoding

-------------------------------------------------------------------------------
-- FromJSON
-------------------------------------------------------------------------------

-- | This instance has to be written by hand, as we wan't to be
-- more lenient, or&and verify that the generic code above works
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
            "task-edit-comment" -> CmdTaskEditComment
                <$> obj .: "eid"
                <*> obj .: "tid"
                <*> obj .: "comment"
            "archive-employee" -> CmdArchiveEmployee
                <$> obj .: "eid"
                <*> obj .: "delete"

            _ -> fail $ "Invalid Command tag " ++ cmd ^. unpacked

-------------------------------------------------------------------------------
-- Rest
-------------------------------------------------------------------------------

instance ToSchema (Command p) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Command") mempty

instance ToJSON (Command f) => Postgres.ToField (Command f) where
    toField = Postgres.toField . Aeson.encode

instance FromJSONField1 f => Postgres.FromField (Command f) where
    fromField f mdata = do
        bs <- Postgres.fromField f mdata
        case Aeson.eitherDecode bs of
            Right x  -> pure x
            Left err -> Postgres.conversionError (Aeson.AesonException err)
