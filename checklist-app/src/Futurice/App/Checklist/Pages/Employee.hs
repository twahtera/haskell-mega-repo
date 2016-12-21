{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Data.Map       as Map
import qualified FUM            (UserName (..))
import qualified Futurice.IdMap as IdMap

-- |
--
-- === Preconditions
--
-- * 'Employee' is in the 'World'.
employeePage
    :: World
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Employee
    -> HtmlPage "employee"
employeePage world authUser employee = checklistPage_ (view nameText employee) authUser $ do
    -- Title
    header (employee ^. nameText) []

    -- Info
    row_ $ large_ 12 $ dl_ $ do
        dt_ "Checklist"
        dd_ $ maybe (pure ()) checklistLink mlist

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

    -- Tasks
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Task" ]  "Task"
            th_ [ title_ "Role" ]  "Role"
            th_ [ title_ "Check" ] "Check"
        tbody_ $ for_ tasks $ \task -> tr_ $ do
            td_ $ taskLink task
            td_ $ roleHtml mlist (task ^. taskRole)
            td_ $ taskCheckbox world employee task
 where
  tasks = toList $ Map.intersection
        (IdMap.toMap (world ^. worldTasks))
        (world ^. worldTaskItems . ix (employee ^. identifier))
  mlist = world ^? worldLists . ix (employee ^. employeeChecklist)
