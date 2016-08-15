{-# LANGUAGE DataKinds      #-}
{--# LANGUAGE TypeOperators  #-}
module Futurice.App.Checklist.API where

import Futurice.Prelude
import Prelude ()

import Servant.API
import Servant.HTML.Lucid (HTML)

import Futurice.App.Checklist.Types.Page

type ChecklistAPI =
    Get '[HTML] (Page "indexpage")

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy
