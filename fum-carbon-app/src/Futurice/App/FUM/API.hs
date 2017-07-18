{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.FUM.API (
    module Futurice.App.FUM.API,
    ) where

import Futurice.Prelude
import Prelude ()

import Futurice.Lomake           (LomakeRequest, LomakeResponse)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.HTML.Lucid        (HTML)

import Futurice.App.FUM.Command

import qualified Personio


type FumCarbonApi = IndexPageEndpoint
    -- Employees
    :<|> CreateEmployeePageEndpoint
    -- reports
    :<|> "reports" :> "validations" :> Get '[HTML] (HtmlPage "validation-report")
    -- commands
    :<|> "commands" :> FumCarbonCommandApi
    -- machine api
    :<|> "api" :> FumCarbonMachineApi

type FumCarbonCommandApi =
    CreateEmployeeCmdEndpoint

type FumCarbonMachineApi =
    "personio-request" :> ReqBody '[JSON] Personio.SomePersonioReq :> Post '[JSON] Personio.SomePersonioRes
    :<|> "raw-employees" :> Get '[JSON] [Personio.Employee]
    :<|> "raw-employee-validations" :> Get '[JSON] [Personio.EmployeeValidation]

fumCarbonApi :: Proxy FumCarbonApi
fumCarbonApi = Proxy

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

type CreateEmployeeCmdEndpoint = "create-employee"
    :> ReqBody '[JSON] (LomakeRequest CommandM CreateEmployee)
    :> Post '[JSON] LomakeResponse

createEmployeeCmdEndpoint :: Proxy ("commands" :> CreateEmployeeCmdEndpoint)
createEmployeeCmdEndpoint = Proxy

-------------------------------------------------------------------------------
-- Index
-------------------------------------------------------------------------------

type IndexPageEndpoint =
    SSOUser :>
    Get '[HTML] (HtmlPage "indexpage")

indexPageEndpoint :: Proxy IndexPageEndpoint
indexPageEndpoint = Proxy

-------------------------------------------------------------------------------
-- Employee
-------------------------------------------------------------------------------

type CreateEmployeePageEndpoint =
    "employees" :> "create" :>
    SSOUser :>
    Capture "personio-id" Personio.EmployeeId :>
    Get '[HTML] (HtmlPage "create-employee")

createEmployeePageEndpoint :: Proxy CreateEmployeePageEndpoint
createEmployeePageEndpoint = Proxy
