{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklists (checklistsPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

checklistsPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "checklists"
checklistsPage world authUser = checklistPage_ "Checklists" authUser $ do
    -- Title
    header "Checklists" []

    -- Table
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Checklist name" ] "Checklist"

        tbody_ $ for_ lists' $ \l -> tr_ $ do
            td_ $ checklistLink l
  where
    -- TODO: sort
    lists' = world ^.. worldLists . folded
