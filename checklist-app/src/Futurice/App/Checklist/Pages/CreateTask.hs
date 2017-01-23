{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateTask (createTaskPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (forOf_, lengthOf, re, to)
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
                "Info"
                input_ [ futuId_ "task-info", type_ "text" ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                select_ [ futuId_ "task-role" ] $ do
                    optionSelected_ True [ value_ "" ] "-"
                    for_ [ minBound .. maxBound ] $ \role ->
                        optionSelected_ False
                            [ value_ $ role ^. re _TaskRole ]
                            $ toHtml $ role ^. re _TaskRole
        row_ $ large_ 12 $ label_ $ do
            "Prerequisites"
            select_ [ futuId_ "task-prereqs", multiple_ "multiple", size_ $ textShow (lengthOf (worldTasks . folded) world) ] $
                forOf_ (worldTasksSorted . folded) world $ \t -> do
                    optionSelected_ False
                        [ value_ $ t ^. identifierText ]
                        $ toHtml $ t ^. nameText
        row_ $ do
            large_ 6 $ label_ $ do
                "Checklist 1"
                checklistSelect "task-checklist-1"
            large_ 6 $ label_ $ do
                "Appliance"
                checklistAppliance "task-checklist-appliance-1"
        row_ $ do
            large_ 6 $ label_ $ do
                "Checklist 2"
                checklistSelect "task-checklist-2"
            large_ 6 $ label_ $ do
                "Appliance"
                checklistAppliance "task-checklist-appliance-2"
        row_ $ do
            large_ 6 $ label_ $ do
                "Checklist 3"
                checklistSelect "task-checklist-3"
            large_ 6 $ label_ $ do
                "Appliance"
                checklistAppliance "task-checklist-appliance-3"

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
  where
    checklistSelect :: Monad m => Text -> HtmlT m ()
    checklistSelect n = select_ [ futuId_ n ] $ do
        option_ [ value_ "" ] $ "-"
        for_ (world ^.. worldLists . folded) $ \cl ->
            optionSelected_ False
                [ value_ $ cl ^. identifier . to identifierToText ]
                $ cl ^. nameHtml

    checklistAppliance :: Monad m => Text -> HtmlT m ()
    checklistAppliance n = input_
        [ futuId_ n, type_ "text", placeholder_ "e.g. not external, helsinki or tampere" ]
