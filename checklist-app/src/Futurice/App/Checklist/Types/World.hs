{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Checklist.Types.World (
    World,
    mkWorld,
    -- * Lenses
    worldEmployees,
    worldTasks,
    worldLists,
    worldTaskItems,
    -- * Getters
    worldTaskItems',
    ) where

-- import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Control.Lens (contains, filtered, ifiltered, (%~))

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Set.Lens   as Set
import qualified Test.QuickCheck as QC

import           Futurice.App.Checklist.Types.Basic
import           Futurice.App.Checklist.Types.Identifier
import           Futurice.App.Checklist.Types.IdMap      (IdMap)
import qualified Futurice.App.Checklist.Types.IdMap      as IdMap

-- | World desribes the state of the db.
data World = World
    { _worldEmployees     :: !(IdMap Employee)
    , _worldTasks     :: !(IdMap Task)
    , _worldLists     :: !(IdMap Checklist)
    , _worldTaskItems :: !(Map (Identifier Employee) (Map (Identifier Task) TaskItemDone))
    -- lazy fields, updated on need when accessed
    , _worldTaskItems' :: Map (Identifier Task) (Map (Identifier Employee) TaskItemDone)
      -- ^ isomorphic with 'worldTaskItems'
    }

makeLenses ''World

-- | Create world from employees, tasks, checklists, and items.
--
-- * Nubs entities by their natural key. /TOOD/
--
-- * Removes 'Task' dependencies to non-existing tasks.
--
-- * Removes non-existing 'Task's from checklists.
--
-- * Removes 'TaskItem's with non-existing employees or tasks.
--
mkWorld
    :: IdMap Employee
    -> IdMap Task
    -> IdMap Checklist
    -> Map (Identifier Employee) (Map (Identifier Task) TaskItemDone)
    -> World
mkWorld us ts ls is =
    let tids            = IdMap.keysSet ts
        cids            = IdMap.keysSet ls
        -- Validation predicates
        validTid tid     = tids ^. contains tid
        validCid cid     = cids ^. contains cid

        -- Cleaned up inputs
        us' = us
            & IdMap.toIdMapOf (folded . filtered (\u -> validCid $ u ^. employeeChecklist))

        ts' = ts
            & IdMap.unsafeTraversal . taskDependencies
            %~ Set.setOf (folded . filtered validTid)

        ls' = ls
            & IdMap.unsafeTraversal . checklistTasks
            %~ toMapOf (ifolded . ifiltered (\k _v -> validTid k))

        -- TODO: create extra fields
    in World us' ts' ls' is (swapMapMap is)

-- | Generates consistent worlds.
instance QC.Arbitrary World where
    arbitrary = do
        -- Generate raw data
        es <- QC.arbitrary
        ts <- QC.arbitrary

        let eids = IdMap.keysSet es
            tids = IdMap.keysSet ts
            eidGen = QC.elements (toList eids)
            tidGen = QC.elements (toList tids)

        let checklistItemGen = (,)
                <$> tidGen
                <*> QC.arbitrary

        cs <- fmap IdMap.fromFoldable . QC.listOf1 $ Checklist
            <$> QC.arbitrary
            <*> QC.arbitrary
            <*> fmap Map.fromList (QC.listOf1 checklistItemGen)

        let cids = IdMap.keysSet cs
            cidGen = QC.elements (toList cids)

        -- Employees
        es' <- flip IdMap.unsafeTraversal es $ \employee -> do
            firstName   <- QC.elements ["Mikko", "Antti", "Ville", "Teemu", "Timo", "Anni", "Laura"]
            lastName    <- QC.elements ["Kikka", "Kukka", "Kukko", "Korhonen", "Virtanen", "Nieminen", "Laine"]
            cid         <- cidGen
            startingDay <- toEnum <$> QC.choose
                (fromEnum $(mkDay "2016-08-01"), fromEnum $(mkDay "2017-01-01"))
            pure $ employee
                & employeeChecklist   .~ cid
                & employeeFirstName   .~ firstName
                & employeeLastName    .~ lastName
                & employeeStartingDay .~ startingDay

        -- Tasks
        -- TODO: we can still generate cyclic tasks!
        ts' <- flip IdMap.unsafeTraversal ts $ \task -> do
            deps <- Set.fromList <$> QC.listOf tidGen
            pure $ task
                & taskDependencies .~ deps

        -- TaskItems
        let taskItemGen = (\eid tid done -> (eid, Map.singleton tid done))
                <$> eidGen <*> tidGen <*> QC.arbitrary
        is <- QC.listOf taskItemGen
        let is' = Map.fromListWith Map.union is

        -- World
        pure $ mkWorld es' ts' cs is'
