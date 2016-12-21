{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Task (taskPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (re)
import Data.Time                 (diffDays)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Data.Map       as Map
import qualified Futurice.IdMap as IdMap

-- |
--
-- === Preconditions
--
-- * 'Task' is in the 'World'.
taskPage
    :: World
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> Task
    -> HtmlPage "task"
taskPage world today authUser task = checklistPage_ (view nameText task <> " - task") authUser $ do
    -- Title
    header (task ^. nameText <> " -  task") []

    -- Edit
    row_ [ id_ "futu-task-edit", data_ "futu-task-id" $ task ^. identifierText ] $ large_ 12 $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                let v = task ^. nameText
                -- TODO: change id to futu-id
                input_ [ id_ "futu-task-name", type_ "text", data_ "futu-value" v, value_ v ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                let v = task ^. taskRole . re _TaskRole
                select_ [ id_ "futu-task-role", data_ "futu-value" v ] $ for_ [ minBound .. maxBound ] $ \role ->
                    optionSelected_ (role == task ^. taskRole)
                        [ value_ $ role ^. re _TaskRole ]
                        $ toHtml $ roleToText role

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Save"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Employees
    subheader_ "Employees"
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Location"]                    "Loc"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Checklist"]                   "List"
            th_ [title_ "Check"]                       "Check"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            th_ [title_ "Days till start"]             "ETA"
        tbody_ $ for_ employees $ \employee -> tr_ $ do
            let startingDay = employee ^. employeeStartingDay
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml (Nothing :: Maybe Checklist) $ employee ^. employeeLocation
            td_ $ employeeLink employee
            -- TODO: checklist link
            td_ $ checklistNameHtml world Nothing $ employee ^. employeeChecklist
            td_ $ taskCheckbox world employee task
            td_ $ toHtml $ show startingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
            td_ $ toHtml $ show (diffDays startingDay today) <> " days"

  where
    employees =  sortOn (view employeeStartingDay) $ toList $ Map.intersection
        (IdMap.toMap (world ^. worldEmployees))
        (world ^. worldTaskItems' .ix (task ^. identifier))
