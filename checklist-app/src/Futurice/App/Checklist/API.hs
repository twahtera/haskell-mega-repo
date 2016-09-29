{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant   (SSOUser)
import Servant.API
import Servant.HTML.Lucid (HTML)

import Futurice.App.Checklist.Types      (Location, TaskRole)
import Futurice.App.Checklist.Types.Page (Page)

type ChecklistAPI = IndexPageEndpoint
    :<|> TasksPageEndpoint

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy

type IndexPageEndpoint =
    SSOUser :>
    QueryParam "location" Location :>
    QueryParam "checklist" UUID :>
    QueryParam "task" UUID :>
    Get '[HTML] (Page "indexpage")

type TasksPageEndpoint =
    "tasks" :>
    SSOUser :>
    QueryParam "role" TaskRole :>
    QueryParam "checklist" UUID :>
    Get '[HTML] (Page "tasks")

indexPageEndpoint :: Proxy IndexPageEndpoint
indexPageEndpoint = Proxy

tasksPageEndpoint :: Proxy TasksPageEndpoint
tasksPageEndpoint = Proxy
