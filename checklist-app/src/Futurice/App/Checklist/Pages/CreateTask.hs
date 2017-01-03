{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateTask (createTaskPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (re)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

createTaskPage
    :: World
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "create-task"
createTaskPage _world authUser = checklistPage_ ("Create task") authUser $ do
    -- Title
    header "Create task" []

    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "task-create" ] $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                input_ [ futuId_ "task-name", type_ "text" ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                let r = TaskRoleIT
                let v = r ^. re _TaskRole
                select_ [ futuId_ "task-role", data_ "futu-value" v ] $ for_ [ minBound .. maxBound ] $ \role ->
                    optionSelected_ (role == r)
                        [ value_ $ role ^. re _TaskRole ]
                        $ toHtml $ role ^. re _TaskRole

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
