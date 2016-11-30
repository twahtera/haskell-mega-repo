{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Task (taskPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (re)
import Data.Time                 (diffDays)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Data.Map       as Map
import qualified FUM            (UserName (..))
import qualified Futurice.IdMap as IdMap

-- |
--
-- === Preconditions
--
-- * 'Task' is in the 'World'.
taskPage
    :: World
    -> Day                                   -- ^ today
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Task
    -> HtmlPage "task"
taskPage world today authUser task = page_ (view nameText task <> " - Checklist") pageParams $ do
    navigation authUser

    -- Title
    header (task ^. nameText) []

    -- Edit
    row_ $ large_ 12 $ form_ $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                input_ [ type_ "text", value_ $ (task ^. nameText) ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                select_ [] $ for_ [ minBound .. maxBound ] $ \role ->
                    optionSelected_ (role == task ^. taskRole)
                        [ value_ $ role ^. re _TaskRole ]
                        $ toHtml $ roleToText role

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success" ] $ "Save"
            button_ [ class_ "button" ] $ "Reset"

    -- Employees
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