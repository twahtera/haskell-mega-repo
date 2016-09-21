{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Checklist.Types.World (
    World,
    mkWorld,
    -- * Lenses
    worldUsers,
    worldTasks,
    worldLists,
    worldTaskItems,
    ) where

-- import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Control.Lens     (contains, filtered, folded, (%~))
import Data.Vector.Lens (toVectorOf, vector)

import qualified Test.QuickCheck as QC

import           Futurice.App.Checklist.Types.Basic
import           Futurice.App.Checklist.Types.IdMap       (IdMap)
import qualified Futurice.App.Checklist.Types.IdMap       as IdMap

-- | World desribes the state of the db.
data World = World
    { _worldUsers     :: !(IdMap User)
    , _worldTasks     :: !(IdMap Task)
    , _worldLists     :: !(IdMap Checklist)
    , _worldTaskItems :: !(IdMap TaskItem)
    -- lazy fields, updated on need when accessed
    }

makeLenses ''World

-- | Create world from users, tasks, checklists, and items.
--
-- * Nubs entities by their natural key. /TOOD/
--
-- * Removes 'Task' dependencies to non-existing tasks.
--
-- * Removes non-existing 'Task's from checklists.
--
-- * Removes 'TaskItem's with non-existing users or tasks.
--
mkWorld
    :: IdMap User
    -> IdMap Task
    -> IdMap Checklist
    -> IdMap TaskItem
    -> World
mkWorld us ts ls is =
    let uids            = IdMap.keysSet us
        tids            = IdMap.keysSet ts
        cids            = IdMap.keysSet ls
        -- Validation predicates
        validUid uid     = uids ^. contains uid
        validTid tid     = tids ^. contains tid
        validCid cid     = cids ^. contains cid
        validTaskItem ti =
            validTid (ti ^. taskItemTask) && validUid (ti ^. taskItemUser)

        -- Cleaned up inputs
        us' = us
            & IdMap.toIdMapOf (folded . filtered (\u -> validCid $ u ^. userChecklist))

        ts' = ts
            & IdMap.unsafeTraversal . taskDependencies
            %~ toVectorOf (folded . filtered validTid)

        ls' = ls
            & IdMap.unsafeTraversal . checklistTasks
            %~ toVectorOf (folded . filtered (validTid . fst))

        is' = is
            & IdMap.toIdMapOf (folded . filtered validTaskItem)

        -- TODO: create extra fields
    in World us' ts' ls' is'

-- | Generates consistent worlds.
instance QC.Arbitrary World where
    arbitrary = do
        -- Generate raw data
        us <- QC.arbitrary
        ts <- QC.arbitrary

        let uids = IdMap.keysSet us
            tids = IdMap.keysSet ts
            uidGen = QC.elements (toList uids)
            tidGen = QC.elements (toList tids)

        let checklistItemGen = (,)
                <$> tidGen
                <*> QC.arbitrary

        cs <- fmap IdMap.fromFoldable . QC.listOf1 $ Checklist
            <$> QC.arbitrary
            <*> QC.arbitrary
            <*> fmap (view vector) (QC.listOf1 checklistItemGen)

        let cids = IdMap.keysSet cs
            cidGen = QC.elements (toList cids)

        -- Users
        us' <- flip IdMap.unsafeTraversal us $ \user -> do
            firstName   <- QC.elements ["Mikko", "Antti", "Ville", "Anni"]
            lastName    <- QC.elements ["Kikka", "Kukka", "Kukko"]
            cid         <- cidGen
            startingDay <- toEnum <$> QC.choose
                (fromEnum $(mkDay "2016-08-01"), fromEnum $(mkDay "2017-01-01"))
            pure $ user
                & userChecklist   .~ cid
                & userFirstName   .~ firstName
                & userLastName    .~ lastName
                & userStartingDay .~ startingDay

        -- Tasks
        -- TODO: we can still generate cyclic tasks!
        ts' <- flip IdMap.unsafeTraversal ts $ \task -> do
            deps <- QC.listOf tidGen
            pure $ task
                & taskDependencies .~ deps ^. vector

        -- TaskItems
        is <- fmap IdMap.fromFoldable . QC.listOf $ TaskItem
            <$> QC.arbitrary
            <*> uidGen
            <*> tidGen
            <*> QC.arbitrary

        -- World
        pure $ mkWorld us' ts' cs is
