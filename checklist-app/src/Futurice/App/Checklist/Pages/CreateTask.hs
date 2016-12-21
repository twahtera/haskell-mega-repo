{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateTask (createTaskPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (re)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

-- |
--
-- === Preconditions
--
-- * 'Task' is in the 'World'.
createTaskPage
    :: World
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "create-task"
createTaskPage _world authUser = checklistPage_ ("Create task") authUser $ do
    -- Title
    header "Create task" []

    -- Edit
    row_ [ id_ "futu-task-new" ] $ large_ 12 $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                -- TODO: change id to futu-id
                input_ [ id_ "futu-task-name", type_ "text" ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                let r = TaskRoleIT
                let v = r ^. re _TaskRole
                select_ [ id_ "futu-task-role", data_ "futu-value" v ] $ for_ [ minBound .. maxBound ] $ \role ->
                    optionSelected_ (role == r)
                        [ value_ $ role ^. re _TaskRole ]
                        $ toHtml $ roleToText role

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
