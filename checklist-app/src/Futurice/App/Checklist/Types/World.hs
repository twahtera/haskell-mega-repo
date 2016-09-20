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

import qualified Data.Map        as Map
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
    let us'             = mapBy (view identifier) us
        ts'             = mapBy (view taskName) ts
        uids            = Map.keysSet us'
        tids            = Map.keysSet ts'
        -- Validation predicates
        validUid uid     = uids ^. contains uid
        validTid tid     = tids ^. contains tid
        validTaskItem ti =
            validTid (ti ^. taskName) && validUid (ti ^. taskItemUser)
        -- Cleaned up inputs
        us'' = toVectorOf folded us'
        ts'' = ts
            & traverse . taskDependencies
            %~ toVectorOf (folded . filtered validTid)
        ls' = ls
            & traverse . checklistTasks
            %~ toVectorOf (folded . filtered (validTid . fst))
        is' = is
            & toVectorOf (folded . filtered validTaskItem)
        -- Extra fields
        -- ...
    in World us'' ts'' ls' is'

-- | Generates consistent worlds.
instance QC.Arbitrary World where
    arbitrary = do
        uids <- nub <$> QC.listOf1 QC.arbitrary
        tids <- nub <$> QC.listOf1 QC.arbitrary
        let uidGen = QC.elements uids
        let tidGen = QC.elements tids

        -- Users
        us <- for uids $ \uid -> do
            user        <- QC.arbitrary
            firstName   <- QC.elements ["Mikko", "Antti", "Ville", "Anni"]
            lastName    <- QC.elements ["Kikka", "Kukka", "Kukko"]
            startingDay <- toEnum <$> QC.choose
                (fromEnum $(mkDay "2016-08-01"), fromEnum $(mkDay "2017-01-01"))
            pure $ user
                & identifier      .~ uid
                & userFirstName   .~ firstName
                & userLastName    .~ lastName
                & userStartingDay .~ startingDay

        -- Tasks
        ts <- for tids $ \tid -> do
            task <- QC.arbitrary
            deps <- QC.listOf tidGen
            pure $ task
                & taskName .~ tid
                & taskDependencies .~ deps ^. vector

        -- Checklists
        let clTaskGen :: QC.Gen (Vector (Name Task, Maybe TaskAppliance))
            clTaskGen = view vector <$> QC.listOf ((,) <$> tidGen <*> QC.arbitrary)
        ls <- QC.listOf $
            Checklist <$> QC.arbitrary <*> clTaskGen

        -- TaskItems
        iids <- nub <$> QC.listOf QC.arbitrary
        is <- for iids $ \iid -> TaskItem iid
            <$> uidGen
            <*> tidGen
            <*> QC.arbitrary

        -- World
        return $ mkWorld
            (us ^. vector)
            (ts ^. vector)
            (ls ^. vector)
            (is ^. vector)

mapBy
    :: (Foldable f, Ord k)
    => (v -> k)  -- ^ Function to extract key, @g@
    -> f v       -- ^ values
    -> Map k v   -- ^ Map with invariant for all @k, v@ pairs @k = g v@
mapBy g = Map.fromList . map (\x -> (g x, x)) . toList
