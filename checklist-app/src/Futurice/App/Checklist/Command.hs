{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.Command (
    -- * Command
    Command (..),
    traverseCommand,
    applyCommand,
    -- * Edits
    TaskEdit (..),
    applyTaskEdit,
    ) where

import Prelude ()
import Futurice.Prelude

import qualified Control.Lens as Lens

import Futurice.App.Checklist.Types

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

-- | Command as in CQRS
data Command f
    = CmdCreateChecklist (f :$ Identifier Checklist) (Name Checklist)
    | CmdRenameChecklist (Identifier Checklist) (Name Checklist)
    | CmdCreateTask (f :$ Identifier Task) TaskEdit
    | CmdEditTask (Identifier Task) TaskEdit
    | CmdAddTask (Identifier Checklist) (Identifier Task) TaskAppliance

traverseCommand
    :: Applicative m
    => (forall x. f (Identifier x) -> m (g (Identifier x)))
    -> Command f
    -> m (Command g)
traverseCommand  f (CmdCreateChecklist i n) =
    CmdCreateChecklist <$> f i <*> pure n
traverseCommand _f (CmdRenameChecklist i n) =
    pure $ CmdRenameChecklist i n
traverseCommand  f (CmdCreateTask i e) =
    CmdCreateTask <$> f i <*> pure e
traverseCommand _f (CmdEditTask i e) =
    pure $ CmdEditTask i e
traverseCommand _f (CmdAddTask c t a) =
    pure $ CmdAddTask c t a

-- todo: in error monad, if e.g. identifier don't exist
applyCommand :: Command I -> World -> World
applyCommand _ = id

-------------------------------------------------------------------------------
-- Edit types
-------------------------------------------------------------------------------

data TaskEdit = TaskEdit
    { teName :: !(Maybe :$ Name Task)
    , teRole :: !(Maybe TaskRole)
    }

applyTaskEdit :: TaskEdit -> Task -> Task
applyTaskEdit te
    = maybe id (Lens.set taskName) (teName te)
    . maybe id (Lens.set taskRole) (teRole te)
