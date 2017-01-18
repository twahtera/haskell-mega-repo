{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateTask (createTaskPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (re, to)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

createTaskPage
    :: World
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "create-task"
createTaskPage world authUser = checklistPage_ ("Create task") authUser $ do
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
        row_ $ do
            large_ 12 $ label_ $ do
                "Checklist 1"
                checklistSelect "task-checklist-1"
            --large_ 6 $ label_ $ do
            --    "Appliance, not implemented"
        row_ $ do
            large_ 12 $ label_ $ do
                "Checklist 2"
                checklistSelect "task-checklist-2"
            --large_ 6 $ label_ $ do
            --    "Appliance, not implemented"
        row_ $ do
            large_ 12 $ label_ $ do
                "Checklist 3"
                checklistSelect "task-checklist-3"
            --large_ 6 $ label_ $ do
            --    "Appliance, not implemented"

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
  where
    checklistSelect :: Monad m => Text -> HtmlT m ()
    checklistSelect n = select_ [ name_ n ] $ do
        option_ [ value_ "" ] $ "-"
        for_ (world ^.. worldLists . folded) $ \cl ->
            optionSelected_ False
                [ value_ $ cl ^. identifier . to identifierToText ]
                $ cl ^. nameHtml
