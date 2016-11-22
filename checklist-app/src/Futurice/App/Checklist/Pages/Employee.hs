{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified FUM (UserName (..))

employeePage
    :: World
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Employee
    -> HtmlPage "employee"
employeePage world authUser employee = page_ (view nameText employee <> " - Checklist") pageParams $ do
    navigation authUser

    -- Title
    header (employee ^. nameText) []

    -- Info
    row_ $ large_ 12 $ dl_ $ do
        dt_ "Checklist"
        dd_ $ maybe (pure ()) checklistLink $
            world ^? worldLists . ix (employee ^. employeeChecklist)

    -- Edit
    row_ $ large_ 12 $ form_ $ do
        row_ $ large_ 12 $ do
            label_ $ do
                "First name"
                input_ [ type_ "text", value_ $ (employee ^. employeeFirstName) ]

            label_ $ do
                "Last name"
                input_ [ type_ "text", value_ $ (employee ^. employeeLastName) ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success" ] $ "Save"
            button_ [ class_ "button" ] $ "Reset"
