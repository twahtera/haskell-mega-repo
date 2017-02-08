{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursApi (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM (newTVarIO)
import Futurice.Integrations
import Futurice.Servant
import Servant

import Futurice.App.FutuhoursApi.API
import Futurice.App.FutuhoursApi.Config
import Futurice.App.FutuhoursApi.Ctx
import Futurice.App.FutuhoursApi.Logic
       (entryDeleteEndpoint, entryEndpoint, entryIdEndpoint, hoursEndpoint,
       projectEndpoint, userEndpoint)

import qualified Data.HashMap.Strict as HM
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

server :: Ctx -> Server FutuhoursAPI
server ctx = pure "This is futuhours api"
    :<|> projectEndpoint ctx
    :<|> userEndpoint ctx
    :<|> hoursEndpoint ctx
    :<|> entryEndpoint ctx
    :<|> entryIdEndpoint ctx
    :<|> entryDeleteEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "Futuhours API"
    & serverDescription     .~ "Here we mark hours"
    & serverApp futuhoursApi .~ server
    -- TODO: remove this before going live:
    & serverMiddleware      .~ liftFuturiceMiddleware logStdoutDev
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverEnvPfx          .~ "FUTUHOURSAPI"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx config logger _cache = do
        now <- currentTime
        mgr <- newManager tlsManagerSettings
        let integrConfig = makeIntegrationsConfig now logger mgr config
        pmData <- fetchPlanmillData integrConfig
        pmDataTVar <- newTVarIO pmData
        pure $ flip (,) [] $ Ctx
            { ctxPlanmillData = pmDataTVar
            , ctxPlanmillCfg  = cfgPlanmillCfg config
            , ctxMockUser     = cfgMockUser config
            , ctxLogger       = logger
            }

    fetchPlanmillData :: IntegrationsConfig I I Proxy Proxy -> IO PlanmillData
    fetchPlanmillData integrConfig = runIntegrations integrConfig $ do
        pmLookupMap <- fumPlanmillMap
        ps <- PMQ.projects
        ps' <- for (toList ps) $ \p -> mkP p <$> PMQ.projectTasks (p ^. PM.identifier)
        let ps'' = HM.fromList ps'
        let ts = HM.fromList $ (\t -> (t ^. PM.identifier, t)) <$>
                ps' ^.. folded . _2 ._2 . folded
        pure $ PlanmillData
            { _planmillUserLookup = pmLookupMap
            , _planmillProjects   = ps''
            , _planmillTasks      = ts
            }
      where
        mkP p ts = (p ^. PM.identifier, (p, toList ts))

makeIntegrationsConfig
    :: UTCTime -> Logger -> Manager -> Config
    -> IntegrationsConfig I I Proxy Proxy
makeIntegrationsConfig now lgr mgr Config {..} = MkIntegrationsConfig
    { integrCfgManager                  = mgr
    , integrCfgNow                      = now
    , integrCfgLogger                   = lgr
    -- Planmill
    , integrCfgPlanmillProxyBaseRequest = I cfgPlanmillProxyReq
    -- FUM
    , integrCfgFumAuthToken             = I cfgFumToken
    , integrCfgFumBaseUrl               = I cfgFumBaseurl
    , integrCfgFumEmployeeListName      = I cfgFumList
    -- GitHub
    , integrCfgGithubProxyBaseRequest   = Proxy
    , integrCfgGithubOrgName            = Proxy
    -- Flowdock
    , integrCfgFlowdockToken            = Proxy
    , integrCfgFlowdockOrgName          = Proxy
    }


