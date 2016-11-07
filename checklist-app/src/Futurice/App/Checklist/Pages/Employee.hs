{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Futurice.Prelude
import Prelude ()
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified FUM (UserName (..))

-- |
--
-- === Preconditions
--
-- * 'Employee' is in the 'World'.
employeePage
    :: World
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Employee
    -> HtmlPage "employee"
employeePage _world authUser employee =
    page_ (view nameText employee <> " - Checklist") pageParams $ contents
  where
    contents = do
        navigation authUser

        -- Title
        header (employee ^. nameText) []

        -- Edit
        row_ $ large_ 12 $ "TODO"

