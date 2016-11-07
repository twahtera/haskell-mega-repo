{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.HTML.Lucid        (HTML)

import Futurice.App.Checklist.Types (Location, TaskRole)

type ChecklistAPI = IndexPageEndpoint
    :<|> ChecklistPageEndpoint
    :<|> TasksPageEndpoint

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy

type IndexPageEndpoint =
    SSOUser :>
    QueryParam "location" Location :>
    QueryParam "checklist" UUID :>
    QueryParam "task" UUID :>
    Get '[HTML] (HtmlPage "indexpage")

type ChecklistPageEndpoint =
    SSOUser :>
    "checklists" :>
    Capture "checklist-id" UUID :>
    Get '[HTML] (HtmlPage "checklist")

type TasksPageEndpoint =
    "tasks" :>
    SSOUser :>
    QueryParam "role" TaskRole :>
    QueryParam "checklist" UUID :>
    Get '[HTML] (HtmlPage "tasks")

indexPageEndpoint :: Proxy IndexPageEndpoint
indexPageEndpoint = Proxy

checklistPageEndpoint :: Proxy ChecklistPageEndpoint
checklistPageEndpoint = Proxy

tasksPageEndpoint :: Proxy TasksPageEndpoint
tasksPageEndpoint = Proxy
