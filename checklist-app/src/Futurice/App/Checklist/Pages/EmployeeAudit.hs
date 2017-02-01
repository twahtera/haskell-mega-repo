{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.EmployeeAudit (employeeAuditPage) where

import Prelude ()
import Futurice.Prelude
--import Control.Lens              (forOf_, has, re)
import Futurice.Lucid.Foundation
import Servant.API               (safeLink)
--import Web.HttpApiData           (toQueryParam)

import Futurice.App.Checklist.API
       (checklistApi, employeePageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Types

employeeAuditPage
    :: World
    -> AuthUser
    -> Employee
    -> [Command Identity]
    -> HtmlPage "employee-audit"
employeeAuditPage _world authUser employee _cmds = checklistPage_ (view nameText employee) authUser $ do
    -- Title
    header (employee ^. nameText) []

    row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ uriText
            $ safeLink checklistApi employeePageEndpoint $ employee ^. identifier
            ]
            "Employee page"
