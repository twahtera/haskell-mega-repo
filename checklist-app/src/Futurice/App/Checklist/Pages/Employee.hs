{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Futurice.Prelude
import Prelude ()
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Pages.Error (notFoundPage)

employeePage
    :: World
    -> UUID -- todo
    -> HtmlPage "employee"
employeePage _ _ = notFoundPage

