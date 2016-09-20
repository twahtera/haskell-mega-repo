{-# LANGUAGE TemplateHaskell        #-}
module Futurice.App.Checklist.Types.World (
    World,
    mkWorld,
    worldValid,
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
import Data.Set.Lens    (setOf)
import Data.Vector.Lens (toVectorOf)

import qualified Test.QuickCheck as QC

import Futurice.App.Checklist.Types.Basic

-- | World desribes the state of the db.
data World = World
    { _worldUsers     :: !(Vector User)
    , _worldTasks     :: !(Vector Task)
    , _worldLists     :: !(Vector Checklist)
    , _worldTaskItems :: !(Vector TaskItem)
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
    :: Vector User
    -> Vector Task
    -> Vector Checklist
    -> Vector TaskItem
    -> World
mkWorld us ts ls is =
    let uids             = setOf (folded . identifier) us
        tids             = setOf (folded . taskName) ts
        -- Validation predicates
        validUid uid     = uids ^. contains uid
        validTid tid     = tids ^. contains tid
        validTaskItem ti =
            validTid (ti ^. taskName) && validUid (ti ^. taskItemUser)
        -- Cleaned up inputs
        ts' = ts
            & traverse . taskDependencies
            %~ toVectorOf (folded . filtered validTid)
        ls' = ls
            & traverse . checklistTasks
            %~ toVectorOf (folded . filtered (validTid . fst))
        is' = is
            & toVectorOf (folded . filtered validTaskItem)
        -- Extra fields
        -- ...
    in World us ts' ls' is'

-- | Check world invariants
--
-- * there should be.. /todo/
worldValid :: World -> Either String World
worldValid = pure

-- | Generates consistent (see 'worldValid') worlds.
instance QC.Arbitrary World where
    -- /TODO:/ make better me
    arbitrary = mkWorld <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
