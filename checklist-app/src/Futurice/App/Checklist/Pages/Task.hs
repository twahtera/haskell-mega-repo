{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Task (taskPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified FUM (UserName (..))

-- |
--
-- === Preconditions
--
-- * 'Task' is in the 'World'.
taskPage
    :: World
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Task
    -> HtmlPage "task"
taskPage _world authUser task = page_ (view nameText task <> " - Checklist") pageParams $ do
    navigation authUser

    -- Title
    header (task ^. nameText) []

    -- Content
    row_ $ large_ 12 $ "TODO"
