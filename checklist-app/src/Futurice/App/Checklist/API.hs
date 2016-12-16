{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.API where

import Prelude ()
import Futurice.Prelude

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.HTML.Lucid        (HTML)

import Futurice.App.Checklist.Command (Command)
import Futurice.App.Checklist.Types
       (Checklist, Employee, Identifier, Location, Task, TaskRole)

type ChecklistAPI = IndexPageEndpoint
    :<|> TasksPageEndpoint
    :<|> ChecklistPageEndpoint
    :<|> TaskPageEndpoint
    :<|> EmployeePageEndpoint
    :<|> "command" :> SSOUser :> ReqBody '[JSON] (Command Proxy) :> Post '[JSON] Text

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy

-------------------------------------------------------------------------------
-- Collections
-------------------------------------------------------------------------------

type IndexPageEndpoint =
    SSOUser :>
    QueryParam "location" Location :>
    QueryParam "checklist" (Identifier Checklist) :>
    QueryParam "task" (Identifier Task) :>
    Get '[HTML] (HtmlPage "indexpage")

type TasksPageEndpoint =
    "tasks" :>
    SSOUser :>
    QueryParam "role" TaskRole :>
    QueryParam "checklist" (Identifier Checklist) :>
    Get '[HTML] (HtmlPage "tasks")

-------------------------------------------------------------------------------
-- Items
-------------------------------------------------------------------------------

type ChecklistPageEndpoint =
    SSOUser :>
    "checklists" :>
    Capture "checklist-id" (Identifier Checklist) :>
    Get '[HTML] (HtmlPage "checklist")

type TaskPageEndpoint =
    SSOUser :>
    "tasks" :>
    Capture "task-id" (Identifier Task) :>
    Get '[HTML] (HtmlPage "task")

type EmployeePageEndpoint =
    SSOUser :>
    "employees" :>
    Capture "employee-id" (Identifier Employee) :>
    Get '[HTML] (HtmlPage "employee")

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

indexPageEndpoint :: Proxy IndexPageEndpoint
indexPageEndpoint = Proxy

tasksPageEndpoint :: Proxy TasksPageEndpoint
tasksPageEndpoint = Proxy

checklistPageEndpoint :: Proxy ChecklistPageEndpoint
checklistPageEndpoint = Proxy

taskPageEndpoint :: Proxy TaskPageEndpoint
taskPageEndpoint = Proxy

employeePageEndpoint :: Proxy EmployeePageEndpoint
employeePageEndpoint = Proxy
