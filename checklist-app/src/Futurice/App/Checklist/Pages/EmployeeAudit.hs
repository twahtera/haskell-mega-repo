{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.EmployeeAudit (employeeAuditPage) where

import Prelude ()
import Futurice.Prelude
--import Control.Lens              (forOf_, has, re)
import Futurice.Lucid.Foundation
import Servant.API               (safeLink)

import Futurice.App.Checklist.API
       (checklistApi, employeePageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Types

import qualified FUM                        (UserName (..))

employeeAuditPage
    :: World
    -> AuthUser
    -> Employee
    -> [(Command Identity, FUM.UserName, UTCTime)]
    -> HtmlPage "employee-audit"
employeeAuditPage _world authUser employee cmds = checklistPage_ (view nameText employee) authUser $ do
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

        for_ cmds $ \(cmd, FUM.UserName username, timestamp) -> tr_ $ do
            td_ $ toHtml $ show $ utcToHelsinkiTime timestamp
            td_ $ toHtml $ username
            td_ $ toHtml $ show cmd
