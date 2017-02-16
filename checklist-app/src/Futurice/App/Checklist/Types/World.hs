{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Checklist.Types.World (
    World,
    emptyWorld,
    mkWorld,
    AuthCheck,
    -- * Lenses
    worldEmployees,
    worldTasks,
    worldLists,
    worldTaskItems,
    -- * Getters
    worldTaskItems',
    worldTasksSorted,
    worldTasksSortedByName,
    ) where

-- import Futurice.Generics
import Prelude ()
import Futurice.Prelude
import Control.Lens     (Getter, contains, filtered, ifiltered, to, (<&>))
import Futurice.Graph   (Graph)
import Futurice.IdMap   (IdMap)

import qualified Data.Set.Lens  as Set
import qualified Futurice.Graph as Graph
import qualified Futurice.IdMap as IdMap

import Futurice.App.Checklist.Types.Basic
import Futurice.App.Checklist.Types.Identifier
import Futurice.App.Checklist.Types.Location
import Futurice.App.Checklist.Types.TaskItem
import Futurice.App.Checklist.Types.TaskRole

{-
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Test.QuickCheck as QC
-}

import qualified FUM

-- | Primitive ACL. Given possible username, return the actual username, role and location.
type AuthCheck = Maybe FUM.UserName -> Maybe (FUM.UserName, TaskRole, Location)

-- | World desribes the state of the db.
data World = World
    { _worldEmployees  :: !(IdMap Employee)
    , _worldTasks      :: !(Graph Task)
    , _worldLists      :: !(IdMap Checklist)
    , _worldTaskItems  :: !(Map (Identifier Employee) (Map (Identifier Task) AnnTaskItem))
      -- ^ ACL lookup
    -- lazy fields, updated on need when accessed
    , _worldTaskItems' :: Map (Identifier Task) (Map (Identifier Employee) AnnTaskItem)
      -- ^ isomorphic with 'worldTaskItems'
    }

worldEmployees :: Lens' World (IdMap Employee)
worldEmployees f (World es ts ls is _) = f es <&>
    \x -> mkWorld x (Graph.toIdMap ts) ls is

worldTasks :: Lens' World (Graph Task)
worldTasks f (World es ts ls is _) = f ts <&>
    \x -> mkWorld es (Graph.toIdMap x) ls is

worldLists :: Lens' World (IdMap Checklist)
worldLists f (World es ts ls is _) = f ls <&>
    \x -> mkWorld es (Graph.toIdMap ts) x is

worldTaskItems :: Lens' World (Map (Identifier Employee) (Map (Identifier Task) AnnTaskItem))
worldTaskItems f (World es ts ls is _) = f is <&>
    \x -> mkWorld es (Graph.toIdMap ts) ls x

worldTaskItems' :: Getter World (Map (Identifier Task) (Map (Identifier Employee) AnnTaskItem))
worldTaskItems' = to _worldTaskItems'

worldTasksSorted :: TaskRole -> Getter World [Task]
worldTasksSorted tr = to $ \world ->
    sortOn ((tr /=) . view taskRole) $ Graph.revTopSort (world ^. worldTasks)

worldTasksSortedByName :: Getter World [Task]
worldTasksSortedByName = to $ \world -> sortOn (view taskName) (world ^.. worldTasks . folded)

emptyWorld :: World
emptyWorld = mkWorld mempty mempty mempty mempty

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
    -> Map (Identifier Employee) (Map (Identifier Task) AnnTaskItem)
    -> World
mkWorld es ts ls is =
    let tids            = IdMap.keysSet ts
        cids            = IdMap.keysSet ls
        -- Validation predicates
        validTid tid     = tids ^. contains tid
        validCid cid     = cids ^. contains cid

        -- Cleaned up inputs
        es' = es
            & IdMap.toIdMapOf (folded . filtered (\u -> validCid $ u ^. employeeChecklist))

        ts' = ts
            & IdMap.unsafeTraversal . taskPrereqs
            %~ Set.setOf (folded . filtered validTid)

        ls' = ls
            & IdMap.unsafeTraversal . checklistTasks
            %~ toMapOf (ifolded . ifiltered (\k _v -> validTid k))

        -- TODO: validate is

        swappedIs = swapMapMap is
    in World es' (Graph.fromIdMap ts') ls' is swappedIs

{-

TODO: AnnTaskItem

-- | Generates consistent worlds.
instance QC.Arbitrary World where
    arbitrary = do
        -- Generate raw data
        es <- QC.arbitrary
        ts <- QC.arbitrary

        let eids = IdMap.keysSet es
            tids = IdMap.keysSet ts
            tidGen = QC.elements (toList tids)

        let checklistItemGen = (,)
                <$> tidGen
                <*> QC.arbitrary

        checkListCount <- QC.choose (5, 10)
        cs <- fmap IdMap.fromFoldable . QC.vectorOf checkListCount $ Checklist
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
                & taskPrereqs .~ deps

        -- AnnTaskItems
        -- For all eid, tid pair generate none, todo, done - value
        let is = [ (eid, tid) | eid <- eids ^.. folded , tid <- tids ^.. folded ]
        is' <- traverse (\p -> (,) p <$> QC.arbitrary) is
        let makeTaskItem ((eid, _tid), Nothing)  = (eid, Map.empty)
            makeTaskItem ((eid, tid), Just done) = (eid, Map.singleton tid done)
        let is'' = Map.fromListWith Map.union (map makeTaskItem is')

        -- World
        pure $ mkWorld es' ts' cs is''
-}
