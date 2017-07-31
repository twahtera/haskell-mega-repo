{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.HoursApi (defaultMain) where

import Control.Concurrent.STM  (newTVarIO, readTVarIO)
import Futurice.Integrations
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Network.HTTP.Client     (managerConnCount)
import Prelude ()
import Servant

import Futurice.App.HoursApi.API
import Futurice.App.HoursApi.Config
import Futurice.App.HoursApi.Ctx
import Futurice.App.HoursApi.Logic
       (entryDeleteEndpoint, entryEditEndpoint, entryEndpoint, hoursEndpoint,
       projectEndpoint, userEndpoint, preferencesEndpoint)
import Futurice.App.HoursApi.Monad (Hours, runHours)

import qualified PlanMill.Worker     as PM
import qualified FUM

server :: Ctx -> Server FutuhoursAPI
server ctx = pure "This is futuhours api"
    :<|> (\mfum        -> authorisedUser ctx mfum projectEndpoint)
    :<|> (\mfum        -> authorisedUser ctx mfum userEndpoint)
    :<|> (\mfum a b    -> authorisedUser ctx mfum (hoursEndpoint a b))
    :<|> (\mfum eu     -> authorisedUser ctx mfum (entryEndpoint eu))
    :<|> (\mfum eid eu -> authorisedUser ctx mfum (entryEditEndpoint eid eu))
    :<|> (\mfum eid    -> authorisedUser ctx mfum (entryDeleteEndpoint eid))
    :<|> (\mfum        -> authorisedUser ctx mfum preferencesEndpoint)

authorisedUser
    :: Ctx
    -> Maybe FUM.UserName
    -> Hours a
    -> Handler a
authorisedUser ctx mfum action =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        pmData <- liftIO $ readTVarIO $ ctxFumPlanmillMap ctx
        (fumUser, pmUser) <- maybe (throwError err403) pure $ pmData ^. at fumUsername
        runHours ctx pmUser (fromMaybe "" $ fumUser ^. FUM.userThumbUrl . lazy) action

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
    makeCtx config lgr cache = do
        now <- currentTime
        mgr <- newManager tlsManagerSettings
            { managerConnCount = 100
            }

        let integrConfig = makeIntegrationsConfig now lgr mgr config
        let getFumPlanmillMap = runIntegrations integrConfig fumPlanmillMap
        let job = mkJob "Update Planmill <- FUM map"  getFumPlanmillMap $ every 600

        fpm <- getFumPlanmillMap
        fpmTVar <- newTVarIO fpm

        let pmCfg = cfgPlanmillCfg config
        ws <- PM.workers lgr mgr pmCfg ["worker1", "worker2", "worker3"]

        pure $ flip (,) [job] Ctx
            { ctxFumPlanmillMap      = fpmTVar
            , ctxPlanmillCfg         = cfgPlanmillCfg config
            , ctxMockUser            = cfgMockUser config
            , ctxManager             = mgr
            , ctxLogger              = lgr
            , ctxCache               = cache
            , ctxPlanMillHaxlBaseReq = cfgPlanmillProxyReq config
            , ctxWorkers             = ws
            }

makeIntegrationsConfig
    :: UTCTime -> Logger -> Manager -> Config
    -> IntegrationsConfig I I Proxy Proxy Proxy
makeIntegrationsConfig now lgr mgr Config {..} = MkIntegrationsConfig
    { integrCfgManager                  = mgr
    , integrCfgNow                      = now
    , integrCfgLogger                   = lgr
    -- Public FUM
    , integrCfgFumPublicUrl             = ""
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
    -- Personio
    , integrCfgPersonioProxyBaseRequest = Proxy
    }
