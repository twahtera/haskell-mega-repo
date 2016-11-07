{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Task (taskPage) where

--import Futurice.Prelude
import Prelude ()
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Pages.Error (notFoundPage)

taskPage
    :: World
    -> Identifier Task
    -> HtmlPage "task"
taskPage _ _ = notFoundPage
