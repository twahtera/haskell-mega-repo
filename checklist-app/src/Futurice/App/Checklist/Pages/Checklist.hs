{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklist (checklistPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

-- |
--
-- === Preconditions
--
-- * 'Checklist' is in the 'World'.
checklistPage
    :: World
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> Checklist
    -> HtmlPage "checklist"
checklistPage _ _ authUser checklist = checklistPage_ (view nameText checklist <> " - checklist") authUser $ do
    header (checklist ^. nameText <> " - checklist") []

    -- Edit

    -- Add Task
    subheader_ "Add task"
    
    -- Tasks
    subheader_ "Tasks"
    
    -- Employees
    subheader_ "Employees"
