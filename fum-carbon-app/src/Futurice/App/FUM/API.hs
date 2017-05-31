{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.FUM.API where

import Prelude ()
import Futurice.Prelude

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.HTML.Lucid        (HTML)

import qualified Personio

type FumCarbonMachineApi =
    "raw-employees" :> Get '[JSON] [Personio.Employee]

type FumCarbonApi = IndexPageEndpoint
    -- Employees
    :<|> CreateEmployeePageEndpoint
    -- machine api
    :<|> "api" :> FumCarbonMachineApi

fumCarbonApi :: Proxy FumCarbonApi
fumCarbonApi = Proxy

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
