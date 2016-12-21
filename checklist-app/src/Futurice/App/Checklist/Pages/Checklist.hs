{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklist (checklistPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (filtered, foldMapOf, has)
import Data.Time                 (diffDays)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Futurice.IdMap as IdMap

-- |
--
-- === Preconditions
--
-- * 'Checklist' is in the 'World'.
checklistPage
    :: World
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> Checklist
    -> HtmlPage "checklist"
checklistPage world today authUser checklist = checklistPage_ (view nameText checklist <> " - checklist") authUser $ do
    header (checklist ^. nameText <> " - checklist") []

    -- Edit

    -- Add Task
    subheader_ "Add task"

    -- Tasks
    subheader_ "Tasks"
    -- TODO: move to Markup: tasksList
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Task" ]                       "Task"
            th_ [ title_ "Role" ]                       "Role"
            th_ [ title_ "Active employees todo/done" ] "Employees"
            th_ [ title_ "Other checklists with the task" ]   "Other checklists"


        tbody_ $ for_ tasks' $ \task -> tr_ $ do

            let tid = task ^. identifier

            td_ $ taskLink task
            td_ $ roleHtml mlist (task ^. taskRole)
            td_ $ a_ [ indexPageHref Nothing mlist (Just tid) ] $
                case foldMapOf (worldTaskItems' . ix tid . folded) countUsers world of
                    TodoCounter _ _ i j ->
                        toHtml (show i) *> "/" *> toHtml (show j)
            td_ $ forWith_
                (br_ [])
                (world ^.. worldLists . folded .  filtered (\l -> has (checklistTasks . ix tid) l))
                checklistLink

    -- Employees
    subheader_ "Employees"
    -- TODO: mvoe to Markup: employeeList
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Location"]                    "Loc"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            th_ [title_ "Days till start"]             "ETA"
        tbody_ $ for_ employees $ \employee -> tr_ $ do
            let startingDay = employee ^. employeeStartingDay
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml (Nothing :: Maybe Checklist) $ employee ^. employeeLocation
            td_ $ employeeLink employee
            td_ $ toHtml $ show startingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
            td_ $ toHtml $ show (diffDays startingDay today) <> " days"


  where
    tasks0 = world ^.. worldTasks . folded
    tasks2 = maybe id (filter . checklistPredicate) mlist tasks0
    tasks' = tasks2

    mlist = Just checklist

    countUsers TaskItemDone = TodoCounter 0 0 1 1
    countUsers TaskItemTodo = TodoCounter 0 0 0 1

    checklistPredicate :: Checklist -> Task -> Bool
    checklistPredicate cl task = flip has world $
        worldLists . ix (cl ^. identifier) . checklistTasks . ix (task ^. identifier)

    employees =  sortOn (view employeeStartingDay)
        $ filter (\e -> e ^. employeeChecklist == checklist ^. identifier)
        $ toList (IdMap.toMap (world ^. worldEmployees))
