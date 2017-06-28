{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.CreateEmployee (createEmployeePage) where

import Futurice.IdMap   (IdMap)
import Futurice.Lomake
import Futurice.Prelude
import Servant.API (safeLink)
import Prelude ()

import Futurice.App.FUM.API
import Futurice.App.FUM.Command
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types  hiding (employeeId)

import qualified Data.Text as T
import qualified Personio

import qualified Data.UUID.Types as UUID

createEmployeePage
    :: World                    -- ^ the world
    -> IdMap Personio.Employee  -- ^ employees
    -> Personio.Employee
    -> HtmlPage "create-employee"
createEmployeePage world _es e = fumPage_ "Create employee" () $ do
    -- Title
    fumHeader_ "Create employee" [] -- TODO: name

    row_ $ large_ 12 $ dl_ $ do
        dt_ "Name"
        dd_ $ toHtml $ e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
        dt_ "Login"
        dd_ $ traverse_ (toHtml . show) $ e ^. Personio.employeeLogin -- TODO: formatting
        dt_ "Hiring date"
        dd_ $ maybe "-" (toHtml . show) $ e ^. Personio.employeeHireDate
        dt_ "Contract end date"
        dd_ $ maybe "-" (toHtml . show) $ e ^. Personio.employeeEndDate

    -- Form
    -- TODO: put into Markup module
    case runCommandM world $ renderBST $ lomakeHtml opts lomakeData createEmployeeLomake of
        Right bs -> toHtmlRaw bs
        Left err -> toHtml $ "ERROR RENDERING FORM:" ++ err
  where
    opts = FormOptions
        { foName = "create-employee-form"
        , foUrl  = safeLink fumCarbonApi createEmployeeCmdEndpoint
        }

    pid = e ^. Personio.employeeId
    lomakeData :: NP I (LomakeFields CreateEmployee)
    lomakeData =
        I (Identifier UUID.nil) :*
        I pid :*
        I loginSuggestion :*
        I Nothing :*
        Nil

    -- TODO: Use data from personio.
    loginSuggestion :: Text
    loginSuggestion = canonicalize $ mconcat
        [ T.take 1 $ e ^. Personio.employeeFirst
        , T.take 3 $ e ^. Personio.employeeLast
        ]
