{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklist (checklistPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay
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
checklistPage _ _ authUser checklist = page_ (view nameText checklist <> " - Checklist") pageParams $ do
    navigation authUser

    -- Title
    header (checklist ^. nameText) []
