{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.Integrations.Monad (
    Integrations,
    Env,
    runIntegrations,
    IntegrationsConfig (..),
    loadIntegrationConfig,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.PlanMill    (MonadPlanMillConstraint (..))
import Data.Constraint
import Futurice.Constraint.Unit1 (Unit1)
import Futurice.EnvConfig
import Futurice.Has              (FlipIn)
import Generics.SOP.Lens         (uni)
import Network.HTTP.Client
       (Request, responseTimeout, responseTimeoutMicro)
import PlanMill.Queries.Haxl     (initDataSourceBatch)

import qualified Chat.Flowdock.REST           as FD
import qualified Flowdock.Haxl                as FD.Haxl
import qualified FUM
import qualified FUM.Haxl
import qualified Futurice.GitHub              as GH
import qualified Futurice.Integrations.GitHub as GH
import qualified Haxl.Core                    as H
import qualified PlanMill.Types.Query         as Q

import Futurice.Integrations.Classes
import Futurice.Integrations.Common

-- | Opaque environment, exported for haddock
--
-- Currently no PlanMill environment.
data Env fum gh fd = Env
    { _envFumEmployeeListName :: !(fum :$ FUM.ListName)
    , _envFlowdockOrgName     :: !(fd :$ FD.ParamName FD.Organisation)
    , _envGithubOrgName       :: !(gh :$ GH.Name GH.Organization)
    , _envNow                 :: !UTCTime
    }

makeLenses ''Env

-- | Integrations monad
--
-- type parameters indicate whether that integration is enabled:
-- 'I' yes, 'Proxy' no
--
newtype Integrations (pm :: * -> *) (fum :: * -> *) (gh :: * -> *) (fd :: * -> *) a
    = Integr { unIntegr :: ReaderT (Env fum gh fd) (H.GenHaxl ()) a }

data IntegrationsConfig pm fum gh fd = MkIntegrationsConfig
    { integrCfgManager                  :: !Manager
    , integrCfgLogger                   :: !Logger
    -- Time
    , integrCfgNow                      :: !UTCTime
    -- Planmill
    , integrCfgPlanmillProxyBaseRequest :: !(pm Request)
    -- FUM
    , integrCfgFumAuthToken             :: !(fum FUM.AuthToken)
    , integrCfgFumBaseUrl               :: !(fum FUM.BaseUrl)
    , integrCfgFumEmployeeListName      :: !(fum FUM.ListName)
    -- GitHub
    , integrCfgGithubProxyBaseRequest   :: !(gh Request)
    , integrCfgGithubOrgName            :: !(gh :$ GH.Name GH.Organization)
    -- Flowdock
    , integrCfgFlowdockToken            :: !(fd FD.AuthToken)
    , integrCfgFlowdockOrgName          :: !(fd :$ FD.ParamName FD.Organisation)
    }

-- | A helper useful in REPL.
loadIntegrationConfig :: Logger -> IO (IntegrationsConfig I I I I)
loadIntegrationConfig lgr = do
    now <- currentTime
    mgr <- newManager tlsManagerSettings
    runLogT "loadIntegrationConfig" lgr $
        getConfig' "REPL" $ MkIntegrationsConfig mgr lgr now
            <$> (f <$$> envVar' "PLANMILLPROXY_HAXLURL")
            <*> envVar' "FUM_TOKEN"
            <*> envVar' "FUM_BASEURL"
            <*> envVar' "FUM_LISTNAME"
            <*> (f <$$> envVar' "GITHUBPROXY_HAXLURL")
            <*> envVar' "GH_ORG"
            <*> envVar' "FD_AUTH_TOKEN"
            <*> envVar' "FD_ORGANISATION"
  where
    f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }
    envVar' :: FromEnvVar a => String -> ConfigParser (I a)
    envVar' = fmap I . envVar

runIntegrations
    :: (SFunctorI pm, SFunctorI fum, SFunctorI gh, SFunctorI fd)
    => IntegrationsConfig pm fum gh fd
    -> Integrations pm fum gh fd a
    -> IO a
runIntegrations cfg (Integr m) = do
    let env = Env
            { _envFumEmployeeListName = integrCfgFumEmployeeListName cfg
            , _envNow                 = integrCfgNow cfg
            , _envFlowdockOrgName     = integrCfgFlowdockOrgName cfg
            , _envGithubOrgName       = integrCfgGithubOrgName cfg
            }
    let haxl = runReaderT m env
    let stateStore
            = pmStateSet
            . fumStateSet
            . fdStateSet
            . ghStateSet
            $ H.stateEmpty
    haxlEnv <- H.initEnv stateStore ()
    H.runHaxl haxlEnv haxl
  where
    mgr         = integrCfgManager cfg
    lgr         = integrCfgLogger cfg
    fumStateSet = extractSEndo $ fmap H.stateSet $ FUM.Haxl.initDataSource' mgr
        <$> integrCfgFumAuthToken cfg
        <*> integrCfgFumBaseUrl cfg
    pmStateSet  = extractSEndo $ fmap H.stateSet $ initDataSourceBatch lgr mgr
        <$> integrCfgPlanmillProxyBaseRequest cfg
    fdStateSet  = extractSEndo $ fmap H.stateSet $ FD.Haxl.initDataSource' mgr
        <$> integrCfgFlowdockToken cfg
    ghStateSet  = extractSEndo $ fmap H.stateSet $ GH.initDataSource lgr mgr
        <$> integrCfgGithubProxyBaseRequest cfg

-------------------------------------------------------------------------------S
-- Functor singletons
-------------------------------------------------------------------------------

data SFunctor f where
    SI :: SFunctor I
    SP :: SFunctor Proxy

class Applicative f => SFunctorI f     where sfunctor :: SFunctor f
instance               SFunctorI I     where sfunctor = SI
instance               SFunctorI Proxy where sfunctor = SP

extractSEndo :: SFunctorI f => f (a -> a) -> a -> a
extractSEndo = extractSFunctor id

extractSFunctor :: forall f a. SFunctorI f => a -> f a -> a
extractSFunctor def f = case sfunctor :: SFunctor f of
    SP -> def
    SI -> f ^. uni

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor (Integrations pm fum gh fd) where
    fmap f (Integr x) = Integr (fmap f x)

instance Applicative (Integrations pm fum gh fd)  where
    pure = Integr . pure
    Integr f <*> Integr x = Integr (f <*> x)
    Integr f  *> Integr x = Integr (f  *> x)

instance Monad (Integrations pm fum gh fd) where
    return = pure
    (>>) = (*>)
    Integr f >>= k = Integr $ f >>= unIntegr . k

-------------------------------------------------------------------------------
-- Monad* instances
-------------------------------------------------------------------------------

instance MonadTime (Integrations pm fum gh fd) where
    currentTime = view envNow

instance pm ~ I => MonadPlanMillConstraint (Integrations pm fum gh fd) where
    type MonadPlanMillC (Integrations pm fum gh fd) = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance pm ~ I => MonadPlanMillQuery (Integrations pm fum gh fd) where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> Integr (lift $ H.dataFetch q)
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

instance fum ~ I  => MonadFUM (Integrations pm fum gh fd) where
    fumAction = Integr . lift . FUM.Haxl.request

instance fd ~ I => MonadFlowdock (Integrations pm fum gh fd) where
    flowdockOrganisationReq = Integr . lift . FD.Haxl.organisation

-------------------------------------------------------------------------------
-- MonadGitHub
-------------------------------------------------------------------------------

instance gh ~ I => MonadGitHub (Integrations pm fum gh fd) where
    type MonadGitHubC (Integrations pm fum gh fd) = FlipIn GH.GHTypes
    githubReq req = case (showDict, typeableDict) of
        (Dict, Dict) -> Integr (lift $ H.dataFetch $ GH.GHR tag req)
      where
        tag = GH.mkTag
        showDict     = GH.tagDict (Proxy :: Proxy Show) tag
        typeableDict = GH.tagDict (Proxy :: Proxy Typeable) tag

-------------------------------------------------------------------------------
-- Has* instances
-------------------------------------------------------------------------------

instance MonadReader (Env fum gh fd) (Integrations pm fum gh fd) where
    ask = Integr ask
    local f = Integr . local f . unIntegr

instance fum ~ I => HasFUMEmployeeListName (Env fum gh fd) where
    fumEmployeeListName = envFumEmployeeListName . uni

instance fd ~ I => HasFlowdockOrgName (Env fum gh fd) where
    flowdockOrganisationName = envFlowdockOrgName . uni

instance (gh ~ I) => HasGithubOrgName (Env fum gh fd) where
    githubOrganisationName = envGithubOrgName . uni
