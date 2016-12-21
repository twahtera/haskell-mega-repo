{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Tasks (tasksPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (filtered, foldMapOf, has, re, to)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified FUM

tasksPage
    :: World                                 -- ^ the world
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Maybe TaskRole
    -> Maybe Checklist
    -> HtmlPage "tasks"
tasksPage world authUser@(_fu, _viewerRole, _viewerLocation) mrole mlist =
    let tasks0 = world ^.. worldTasks . folded
        tasks1 = maybe id (filter . rolePredicate) mrole tasks0
        tasks2 = maybe id (filter . checklistPredicate) mlist tasks1
        tasks' = tasks2

        rolePredicate :: TaskRole -> Task -> Bool
        rolePredicate role task = role == task ^. taskRole

        checklistPredicate :: Checklist -> Task -> Bool
        checklistPredicate cl task = flip has world $
            worldLists . ix (cl ^. identifier) . checklistTasks . ix (task ^. identifier)

    in checklistPage_ "Tasks" authUser $ do
        -- Title
        header "Tasks"
            [ (^. re _TaskRole) <$> mrole
            , (^. nameText ) <$> mlist
            ]

        -- List filtering controls
        row_ $ form_ [ action_ $ "/tasks", method_ "get" ] $ do
            largemed_ 3 $ label_ $ do
                "Role"
                select_ [ name_ "role"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ [ minBound .. maxBound ] $ \role ->
                        optionSelected_ (Just role == mrole)
                            [ value_ $ role ^. re _TaskRole ]
                            $ toHtml $ roleToText role
            largemed_ 8 $ label_ $ do
                "Checklist"
                select_ [ name_ "checklist"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldLists . folded) $ \cl ->
                        optionSelected_ (Just cl == mlist)
                            [ value_ $ cl ^. identifier . to identifierToText ]
                            $ cl ^. nameHtml
            largemed_ 1 $ label_ $ do
                toHtmlRaw ("&nbsp;" :: Text)
                button_ [ class_ "button" ] $ "Filter"

        -- The table
        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ [ title_ "Task" ]                       "Task"
                th_ [ title_ "Role" ]                       "Role"
                th_ [ title_ "Active employees todo/done" ] "Employees"
                th_ [ title_ "Checklists with the task" ]   "Checklists"

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
 where
  countUsers TaskItemDone = TodoCounter 0 0 1 1
  countUsers TaskItemTodo = TodoCounter 0 0 0 1
