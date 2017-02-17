{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.EmployeeAudit (employeeAuditPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens               (ALens, use, ( #~ ), (.=), (^#))
import Control.Monad.State.Strict (evalState)
import Futurice.Lucid.Foundation
import Servant.API                (safeLink)

import Futurice.App.Checklist.API     (checklistApi, employeePageEndpoint)
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified FUM (UserName (..))

employeeAuditPage
    :: World
    -> AuthUser
    -> Employee
    -> [(Command Identity, FUM.UserName, UTCTime)]
    -> HtmlPage "employee-audit"
employeeAuditPage world authUser employee cmds = checklistPage_ (view nameText employee) authUser $ do
    let eid = employee ^. identifier

    -- Title
    header (employee ^. nameText) []

    row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ linkToText
            $ safeLink checklistApi employeePageEndpoint $ employee ^. identifier
            ]
            "Employee page"

    row_ $ large_ 12 $ table_ $ do
        tr_ $ do
            th_ "Who"
            th_ "When"
            th_ "What"

        for_ (processCommands world eid _1 cmds) $ \(cmdHtml, FUM.UserName username, timestamp) -> tr_ $ do
            td_ $ toHtml $ username
            td_ $ toHtml $ formatHumanHelsinkiTime timestamp
            td_ $ cmdHtml

processCommands
    :: World
    -> Identifier Employee
    -> ALens s t (Command Identity) (Html ())
    -> [s]
    -> [t]
processCommands world eid l = concat . flip evalState False . traverse process
  where
    taskHtml tid = maybe (toHtml $ tid ^. identifierText) taskLink $
        world ^? worldTasks . ix tid

    process x  = do
        case x ^# l of
            CmdCreateEmployee (Identity eid') _ edit
                -- if created this employee
                | eid == eid' -> do
                    id .= True
                    pure [ x & l #~ toHtml (textShow edit)]

                -- created someone else, we don't care
                | otherwise -> pure []

            CmdTaskItemToggle _ tid d -> do
                -- showing always
                let html = do
                      b_ $ if d == TaskItemDone then "Task done" else "Task undone"
                      " "
                      taskHtml tid
                pure [ x & l #~ html ]

            CmdTaskEditComment _ tid comment -> do
                -- showing always
                let html = do
                      b_ "Task commented"
                      " "
                      taskHtml tid
                      ": "
                      i_ $ toHtml comment
                pure [ x & l #~ html ]

            CmdAddTask _ tid app -> do
                -- TODO: show only if current state of user applies
                created <- use id

                let html = do
                      b_ "Added task"
                      " "
                      taskHtml tid
                      " with appliance "
                      toHtml app

                pure $ if created then [x & l #~ html ] else []

            -- Default
            cmd -> do
                created <- use id
                let html = toHtml $ textShow cmd
                pure $ if created then [x & l #~ html ] else []
