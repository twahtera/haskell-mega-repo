{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (forOf_, has, re)
import Futurice.Lucid.Foundation
import Servant.API               (safeLink)
import Web.HttpApiData           (toQueryParam, toUrlPiece)

import Futurice.App.Checklist.API
       (checklistApi, checklistPageEndpoint, createEmployeePageEndpoint,
       employeeAuditPageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

-- |
--
-- === Preconditions
--
-- * 'Employee' is in the 'World'.
employeePage
    :: World
    -> AuthUser
    -> Employee
    -> HtmlPage "employee"
employeePage world authUser employee = checklistPage_ (view nameText employee) authUser $ do
    -- Title
    header (employee ^. nameText) []

    -- Buttons
    row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
        for_ mlist $ \cl -> button_
            [ class_ "button"
            , data_ "futu-link-button" $ uriText
            $ safeLink checklistApi checklistPageEndpoint (cl ^. identifier)
            ]
            $ toHtml $ "Checklist: " <> cl ^. nameText
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ toUrlPiece
            $ safeLink checklistApi createEmployeePageEndpoint $ employee ^? identifier
            ]
            "Copy into new employee"
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ uriText
            $ safeLink checklistApi employeeAuditPageEndpoint $ employee ^. identifier
            ]
            "Audit log"

    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "employee-edit", data_ "futu-employee-id" $ employee ^. identifierText ] $ do
        row_ $ large_ 12 $ label_ $ do
            "First name"
            input_ [ futuId_ "employee-firstname", type_ "text", value_ $ employee ^. employeeLastName ]
        row_ $ large_ 12 $ label_ $ do
            "Last name"
            input_ [ futuId_ "employee-lastname", type_ "text", value_ $ employee ^. employeeLastName ]
        row_ $ large_ 12 $ label_ $ do
            "Contract"
            select_ [ futuId_ "employee-contract-type" ] $ for_ [ minBound .. maxBound ] $ \x ->
                optionSelected_ (x == employee ^. employeeContractType)
                    [ value_ $ x ^. re _ContractType ]
                    $ toHtml $ x ^. re _ContractType
        row_ $ large_ 12 $ label_ $ do
            "Location"
            select_ [ futuId_ "employee-location" ] $ for_ [ minBound .. maxBound ] $ \x ->
                optionSelected_ (x == employee ^. employeeLocation)
                    [ value_ $ x ^. re _Location ]
                    $ toHtml $ x ^. re _Location
        row_ $ large_ 12 $ label_ $ do
            "Confirmed"
            br_ []
            checkbox_ (employee ^. employeeConfirmed) [ futuId_ "employee-confirmed" ]
        row_ $ large_ 12 $ label_ $ do
            "Due day"
            input_ [ futuId_ "employee-starting-day", type_ "date", value_ $ toQueryParam $ employee ^. employeeStartingDay  ]
        row_ $ large_ 12 $ label_ $ do
            "Supervisor"
            input_ [ futuId_ "employee-supervisor", type_ "text", value_ $ toQueryParam $ employee ^. employeeSupervisor ]
        row_ $ large_ 12 $ label_ $ do
            "Tribe"
            input_ [ futuId_ "employee-tribe", type_ "text", value_ $ employee ^. employeeTribe ]
        row_ $ large_ 12 $ label_ $ do
            "Info"
            textarea_ [ futuId_ "employee-info" ] $ toHtml $ employee ^. employeeInfo
        row_ $ large_ 12 $ label_ $ do
            "Phone"
            -- TODO: maybe it's simpler to just define empty value
            input_ $ [ futuId_ "employee-phone", type_ "tel" ] ++
                catMaybes [ value_ <$> employee ^. employeePhone ]
        row_ $ large_ 12 $ label_ $ do
            "Private email"
            input_ $ [ futuId_ "employee-contact-email", type_ "email" ] ++
                catMaybes [ value_ <$> employee ^. employeeContactEmail ]
        row_ $ large_ 12 $ label_ $ do
            "FUM handle"
            input_ $ [ futuId_ "employee-fum-login", type_ "text" ] ++
                catMaybes [ value_ . toQueryParam <$> employee ^. employeeFUMLogin ]
        row_ $ large_ 12 $ label_ $ do
            "HR number"
            input_ $ [ futuId_ "employee-hr-number", type_ "text" ] ++
                catMaybes [ value_ . textShow <$> employee ^. employeeHRNumber ]
        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Edit"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Tasks
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Task" ]  "Task"
            th_ [ title_ "Role" ]  "Role"
            th_ [ title_ "Check" ] "Check"
        tbody_ $ forOf_ (worldTasksSorted . folded) world $ \task -> do
            let tid = task ^. identifier
            when (has (worldTaskItems . ix eid . ix tid) world) $ tr_ $ do
                td_ $ taskLink task
                td_ $ roleHtml mlist (task ^. taskRole)
                td_ $ taskCheckbox world employee task
  where
    eid = employee ^. identifier
    mlist = world ^? worldLists . ix (employee ^. employeeChecklist)
