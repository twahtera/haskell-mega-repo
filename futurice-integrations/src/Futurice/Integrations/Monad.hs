{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.Integrations.Monad (
    Integrations,
    Env,
    runIntegrations,
    IntegrationsConfig (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.PlanMill     (MonadPlanMillConstraint (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Constraint
import Futurice.Constraint.Unit1  (Unit1)
import Network.HTTP.Client        (Manager, Request)
import PlanMill.Queries.Haxl      (initDataSourceBatch)

import qualified Chat.Flowdock.REST   as FD
import qualified FUM
import qualified FUM.Haxl
import qualified Flowdock.Haxl as FD.Haxl
import qualified Haxl.Core            as H
import qualified PlanMill.Types.Query as Q

import Futurice.Integrations.Classes
import Futurice.Integrations.Common

-- | Opaque environment, exported for haddock
data Env = Env
    { _envFumEmployeeListName :: !FUM.ListName
    , _envFlowdockOrgName     :: !(FD.ParamName FD.Organisation)
    , _envNow                 :: !UTCTime
    }

makeLenses ''Env

newtype Integrations a = Integr { unIntegr :: ReaderT Env (H.GenHaxl ()) a }

data IntegrationsConfig = MkIntegrationsConfig
    { integrCfgManager                  :: !Manager
    , integrCfgNow                      :: !UTCTime
    -- Planmill
    , integrCfgPlanmillProxyBaseRequest :: !Request
    -- FUM
    , integrCfgFumAuthToken             :: !FUM.AuthToken
    , integrCfgFumBaseUrl               :: !FUM.BaseUrl
    , integrCfgFumEmployeeListName      :: !FUM.ListName
    -- Flowdock
    , integrCfgFlowdockToken            :: !FD.AuthToken
    , integrCfgFlowdockOrgName          :: !(FD.ParamName FD.Organisation)
    }

runIntegrations :: IntegrationsConfig -> Integrations a -> IO a
runIntegrations cfg (Integr m) = do
    let env = Env
            { _envFumEmployeeListName = integrCfgFumEmployeeListName cfg
            , _envNow                 = integrCfgNow cfg
            , _envFlowdockOrgName     = integrCfgFlowdockOrgName cfg
            }
    let haxl = runReaderT m env
    let stateStore
            = H.stateSet (initDataSourceBatch mgr planmillReq)
            $ H.stateSet (FUM.Haxl.initDataSource' mgr fumToken fumBaseUrl)
            $ H.stateSet (FD.Haxl.initDataSource' mgr fdToken)
            $ H.stateEmpty
    haxlEnv <- H.initEnv stateStore ()
    H.runHaxl haxlEnv haxl
  where
    mgr         = integrCfgManager cfg
    planmillReq = integrCfgPlanmillProxyBaseRequest cfg
    fumToken    = integrCfgFumAuthToken cfg
    fumBaseUrl  = integrCfgFumBaseUrl cfg
    fdToken     = integrCfgFlowdockToken cfg

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor Integrations where
    fmap f (Integr x) = Integr (fmap f x)

instance Applicative Integrations where
    pure = Integr . pure
    Integr f <*> Integr x = Integr (f <*> x)
    Integr f  *> Integr x = Integr (f  *> x)

instance Monad Integrations where
    return = pure
    (>>) = (*>)
    Integr f >>= k = Integr $ f >>= unIntegr . k

-------------------------------------------------------------------------------
-- Monad* instances
-------------------------------------------------------------------------------

instance MonadTime Integrations where
    currentTime = view envNow

instance MonadPlanMillConstraint Integrations where
    type MonadPlanMillC Integrations = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance MonadPlanMillQuery Integrations where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> Integr (lift $ H.dataFetch q)
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

instance MonadFUM Integrations where
    fumAction = Integr . lift . FUM.Haxl.request

instance MonadFlowdock Integrations where
    flowdockOrganisationReq = Integr . lift . FD.Haxl.organisation

-------------------------------------------------------------------------------
-- Has* instances
-------------------------------------------------------------------------------

instance MonadReader Env Integrations where
    ask = Integr ask
    local f = Integr . local f . unIntegr

instance HasFUMEmployeeListName Env where
    fumEmployeeListName = envFumEmployeeListName

instance HasFlowdockOrgName Env where
    flowdockOrganisationName = envFlowdockOrgName
