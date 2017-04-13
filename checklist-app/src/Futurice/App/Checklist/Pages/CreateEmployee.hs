{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateEmployee (createEmployeePage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (forOf_, re, to)
import Data.Aeson                (ToJSON)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Set.Lens             (setOf)
import Futurice.Lucid.Foundation
import Web.HttpApiData           (toQueryParam)

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

createEmployeePage
    :: HasValidTribes
    => World
    -> AuthUser    -- ^ logged in user
    -> Maybe Employee
    -> HtmlPage "create-employee"
createEmployeePage world authUser memployee = checklistPage_ ("Create employee") authUser $ do
    -- Title
    header "Create employee" []

    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "employee-create" ] $ do
        row_ $ large_ 12 $ label_ $ do
            "Checklist"
            -- TODO: sort checklists
            select_ [ futuId_ "employee-checklist" ] $ do
                optionSelected_ True [ value_ "" ] "-"
                forOf_ (worldLists . folded) world $ \l ->
                    optionSelected_ False
                        [ value_ $ l ^. identifierText ]
                        $ toHtml $ l ^. nameText
        row_ $ large_ 12 $ label_ $ do
            "First name"
            input_
                [ futuId_ "employee-firstname", type_ "text"
                , value_ $ maybe "" (view employeeFirstName) memployee
                ]
        row_ $ large_ 12 $ label_ $ do
            "Last name"
            input_
                [ futuId_ "employee-lastname", type_ "text"
                , value_ $ maybe "" (view employeeLastName) memployee
                ]
        row_ $ large_ 12 $ label_ $ do
            "Contract"
            let v = view employeeContractType <$> memployee
            select_ [ futuId_ "employee-contract-type" ] $ do
                optionSelected_ (v == Nothing) [ value_ "" ] "-"
                for_ [ minBound .. maxBound ] $ \x ->
                    optionSelected_ (v == Just x)
                        [ value_ $ x ^. re _ContractType ]
                        $ toHtml $ x ^. re _ContractType
        row_ $ large_ 12 $ label_ $ do
            "Location"
            let v = view employeeLocation <$> memployee
            select_ [ futuId_ "employee-location" ] $ do
                optionSelected_ (v == Nothing) [ value_ "" ] "-"
                for_ [ minBound .. maxBound ] $ \x ->
                    optionSelected_ (v == Just x)
                        [ value_ $ x ^. re _Location ]
                        $ toHtml $ x ^. re _Location
        row_ $ large_ 12 $ label_ $ do
            "Confirmed"
            br_ []
            input_ [ futuId_ "employee-confirmed", type_ "checkbox" ]
        row_ $ large_ 12 $ label_ $ do
            "Due day"
            input_ [ futuId_ "employee-starting-day", type_ "date" ]
        row_ $ large_ 12 $ label_ $ do
            "Supervisor"
            input_
                [ futuId_ "employee-supervisor", type_ "text"
                , data_ "futu-values" $ encodeToText supervisors
                , value_ $ maybe "" (toQueryParam . view employeeSupervisor) memployee
                ]
        row_ $ large_ 12 $ label_ $ do
            "Tribe"
            select_ [ futuId_ "employee-tribe", type_ "text" ] $ do
                optionSelected_ False [ value_ "" ] "-"
                forOf_ foldedValidTribes validTribes $ \tribe ->
                    optionSelected_ False
                        [ value_ $ toQueryParam tribe ]
                        $ toHtml tribe
        row_ $ large_ 12 $ label_ $ do
            "Info"
            textarea_ [ futuId_ "employee-info", rows_ "5" ] (pure ())
        row_ $ large_ 12 $ label_ $ do
            "Phone"
            input_
                [ futuId_ "employee-phone", type_ "tel"
                , value_ $ fromMaybe "" $ memployee >>= view employeePhone
                ]
        row_ $ large_ 12 $ label_ $ do
            "Private email"
            input_
                [ futuId_ "employee-contact-email", type_ "email"
                , value_ $ fromMaybe "" $ memployee >>= view employeeContactEmail
                ]
        row_ $ large_ 12 $ label_ $ do
            "FUM handle"
            input_
                [ futuId_ "employee-fum-login", type_ "text"
                , value_ $ maybe "" toQueryParam $ memployee >>= view employeeFUMLogin
                ]
        row_ $ large_ 12 $ label_ $ do
            "HR number"
            input_
                [ futuId_ "employee-hr-number", type_ "text"
                , value_ $ maybe "" textShow $ memployee >>= view employeeHRNumber
                ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
  where
    supervisors :: [Text]
    supervisors = toList $ setOf (worldEmployees . folded . employeeSupervisor . to toQueryParam) world

encodeToText :: ToJSON a => a -> Text
encodeToText = view strict . encodeToLazyText
