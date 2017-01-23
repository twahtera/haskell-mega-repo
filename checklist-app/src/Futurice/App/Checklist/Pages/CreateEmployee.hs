{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateEmployee (createEmployeePage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (forOf_, re)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

createEmployeePage
    :: World
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "create-employee"
createEmployeePage world authUser = checklistPage_ ("Create employee") authUser $ do
    -- Title
    header "Create employee" []

    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "employee-create" ] $ do
        row_ $ large_ 12 $ label_ $ do
            "Checklist"
            -- TODO: sort checklists
            select_ [ futuId_ "employee-checklist" ] $ do
                optionSelected_ True [ value_ "" ] "-"
                forOf_ (worldLists . folded) world $ \l ->
                    optionSelected_ False
                        [ value_ $ l ^. identifierText ]
                        $ toHtml $ l ^. nameText
        row_ $ large_ 12 $ label_ $ do
            "First name"
            input_ [ futuId_ "employee-firstname", type_ "text" ]
        row_ $ large_ 12 $ label_ $ do
            "Last name"
            input_ [ futuId_ "employee-lastname", type_ "text" ]
        row_ $ large_ 12 $ label_ $ do
            "Contract"
            select_ [ futuId_ "employee-contract-type" ] $ do
                optionSelected_ True [ value_ "" ] "-"
                for_ [ minBound .. maxBound ] $ \x ->
                    optionSelected_ False
                        [ value_ $ x ^. re _ContractType ]
                        $ toHtml $ x ^. re _ContractType
        row_ $ large_ 12 $ label_ $ do
            "Location"
            select_ [ futuId_ "employee-location" ] $ do
                optionSelected_ True [ value_ "" ] "-"
                for_ [ minBound .. maxBound ] $ \x ->
                    optionSelected_ False
                        [ value_ $ x ^. re _Location ]
                        $ toHtml $ x ^. re _Location
        row_ $ large_ 12 $ label_ $ do
            "Confirmed"
            br_ []
            input_ [ futuId_ "employee-confirmed", type_ "checkbox" ]
        row_ $ large_ 12 $ label_ $ do
            "Due day"
            input_ [ futuId_ "employee-starting-day", type_ "date" ]
        row_ $ large_ 12 $ label_ $ do
            "Supervisor"
            input_ [ futuId_ "employee-supervisor", type_ "text" ]
        row_ $ large_ 12 $ label_ $ do
            "Tribe"
            input_ [ futuId_ "employee-tribe", type_ "text" ]
        row_ $ large_ 12 $ label_ $ do
            "Info"
            textarea_ [ futuId_ "employee-info" ] (pure ())
        row_ $ large_ 12 $ label_ $ do
            "Phone"
            input_ [ futuId_ "employee-phone", type_ "tel" ]
        row_ $ large_ 12 $ label_ $ do
            "Private email"
            input_ [ futuId_ "employee-contact-email", type_ "email" ]
        row_ $ large_ 12 $ label_ $ do
            "FUM handle"
            input_ [ futuId_ "employee-fum-login", type_ "text" ]
        row_ $ large_ 12 $ label_ $ do
            "HR number"
            input_ [ futuId_ "employee-hr-number", type_ "text" ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
