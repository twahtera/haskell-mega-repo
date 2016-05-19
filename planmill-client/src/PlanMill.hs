-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill (
    mockEndpoint,
    -- * Module re-exports
    module Control.Monad.PlanMill,
    module PlanMill.Auth,
    module PlanMill.Classes,
    module PlanMill.Types,
    module PlanMill.Endpoints,
    module PlanMill.Enumerations,
    ) where

import Control.Monad.PlanMill
import PlanMill.Auth
import PlanMill.Classes
import PlanMill.Endpoints
import PlanMill.Enumerations
import PlanMill.Types

------------------------------------------------------------------------------
-- Mocking endpoint stuff
------------------------------------------------------------------------------

-- | Mock api end point
--
-- TODO: what to do
mockEndpoint :: String
mockEndpoint =
    "http://mocksvc.mulesoft.com/mocks/c837a2fb-4326-4ff1-a1ab-bb0f37297be2/api/1.5"
