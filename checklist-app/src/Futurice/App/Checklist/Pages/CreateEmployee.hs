{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateEmployee (createEmployeePage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

createEmployeePage
    :: World
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "create-employee"
createEmployeePage _world authUser = checklistPage_ ("Create employee") authUser $ do
    -- Title
    header "Create employee" []

    -- Edit
    row_ [ id_ "futu-employee-new" ] $ large_ 12 $ do
        row_ $ large_ 12 $
            label_ $ do
                "First name"
                -- TODO: change id to futu-id
                input_ [ id_ "futu-employee-firstname", type_ "text" ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
