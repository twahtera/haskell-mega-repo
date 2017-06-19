{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM.Pages.Index (indexPage) where

import Prelude ()
import Futurice.Prelude
import Futurice.IdMap (IdMap)

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types hiding (employeeId)

import qualified Personio

indexPage
    :: World                    -- ^ the world
    -> IdMap Personio.Employee  -- ^ employees
    -> HtmlPage "indexpage"
indexPage _world es = fumPage_ "FUM" () $ do
    subheader_ "Personio users"
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ "id"
            th_ "first"
            th_ "last"
            th_ "login"
            th_ "hire date"
            th_ "end date"
            th_ "create"
        tbody_ $ for_ es $ \Personio.Employee {..} -> tr_ $ do
            td_ $ toHtml $ show _employeeId
            td_ $ toHtml _employeeFirst
            td_ $ toHtml _employeeLast
            td_ $ traverse_ toHtml _employeeLogin
            td_ $ traverse_ (toHtml . show) _employeeHireDate
            td_ $ traverse_ (toHtml . show) _employeeEndDate
            td_ $ futuLinkButton_ (createEmployeeHref_ _employeeId) "Create"
