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
    | CmdCreateTask (f :$ Identifier Task) (TaskEdit Identity)
    | CmdEditTask (Identifier Task) (TaskEdit Maybe)
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
applyCommand :: Command Identity -> World -> World
applyCommand _ = id

instance Show1 f => Show (Command f) where
    showsPrec d (CmdCreateChecklist i n) = showsBinaryWith
        showsPrec1 showsPrec
        "CmdCreateChecklist" d i n
    showsPrec d (CmdRenameChecklist i n) = showsBinaryWith
        showsPrec showsPrec
        "CmdRenameChecklist" d i n
    showsPrec d (CmdCreateTask i te) = showsBinaryWith
        showsPrec1 showsPrec
        "CmdCreateTask" d i te
    showsPrec d (CmdEditTask i te) = showsBinaryWith
        showsPrec showsPrec
        "CmdEditTask" d i te
    showsPrec d (CmdAddTask c t a) = showsTernaryWith
        showsPrec showsPrec showsPrec
        "CmdAddTask" d c t a

-------------------------------------------------------------------------------
-- Edit types
-------------------------------------------------------------------------------

data TaskEdit f = TaskEdit
    { teName :: !(f :$ Name Task)
    , teRole :: !(f TaskRole)
    }

applyTaskEdit :: TaskEdit Maybe -> Task -> Task
applyTaskEdit te
    = maybe id (Lens.set taskName) (teName te)
    . maybe id (Lens.set taskRole) (teRole te)

instance Show1 f => Show (TaskEdit f) where
    showsPrec d (TaskEdit n r) = showsBinaryWith
        showsPrec1 showsPrec1
        "TaskEdit" d n r
