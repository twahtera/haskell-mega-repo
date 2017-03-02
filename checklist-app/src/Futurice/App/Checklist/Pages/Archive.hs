{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Archive (archivePage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

archivePage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "archive"
archivePage world authUser = checklistPage_ "Employees" authUser $ do
    let employees = sortOn (view employeeStartingDay) $ world ^.. worldArchive . folded

    -- Title
    header "Archive" []

    -- The table
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Location"]                    "Loc"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Checklist"]                   "List"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
        tbody_ $ for_ employees $ \employee -> tr_ $ do
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml (Nothing :: Maybe Checklist) $ employee ^. employeeLocation
            td_ $ employee ^. nameHtml
            td_ $ checklistNameHtml world Nothing (employee ^. employeeChecklist) False
            td_ $ toHtml $ show $ employee ^. employeeStartingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
