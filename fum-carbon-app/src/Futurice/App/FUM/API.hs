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
    -- commands
    :<|> "commands" :> FumCarbonCommandApi
    -- machine api
    :<|> "api" :> FumCarbonMachineApi

type FumCarbonCommandApi =
    CreateEmployeeCmdEndpoint

type FumCarbonMachineApi =
    "raw-employees" :> Get '[JSON] [Personio.Employee]

fumCarbonApi :: Proxy FumCarbonApi
fumCarbonApi = Proxy

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

type CreateEmployeeCmdEndpoint = "create-employee"
    :> ReqBody '[JSON] (LomakeRequest CreateEmployee)
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
