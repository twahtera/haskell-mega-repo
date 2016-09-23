{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant   (SSOUser)
import Servant.API
import Servant.HTML.Lucid (HTML)

import Futurice.App.Checklist.Types      (Location)
import Futurice.App.Checklist.Types.Page (Page)

type ChecklistAPI = IndexPageEndpoint

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy

type IndexPageEndpoint =
    SSOUser :>
    QueryParam "location" Location :>
    QueryParam "checklist" UUID :>
    QueryParam "task" UUID :>
    Get '[HTML] (Page "indexpage")
