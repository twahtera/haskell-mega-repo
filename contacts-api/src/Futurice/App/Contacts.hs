{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Contacts (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Integrations
import Futurice.Periocron
import Futurice.Servant
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant

-- Contacts modules
import Futurice.App.Contacts.API
import Futurice.App.Contacts.Config (Config (..))
import Futurice.App.Contacts.Logic  (contacts)
import Futurice.App.Contacts.Types

type Ctx = IO [Contact Text]

server :: Ctx -> Server ContactsAPI
server action = liftIO action :<|> liftIO action

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "Contacts API"
    & serverDescription     .~ "All employees and externals"
    & serverApp contactsApi .~ server
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF2 'AC3))
    & serverEnvPfx          .~ "CONTACTSAPI"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx Config {..} logger cache = do
        mgr <- newManager tlsManagerSettings
        now <- currentTime
        let cfg = MkIntegrationsConfig
                { integrCfgManager                  = mgr
                , integrCfgLogger                   = logger
                , integrCfgNow                      = now
                -- Planmill
                , integrCfgPlanmillProxyBaseRequest = cfgPmBaseReq
                -- FUM
                , integrCfgFumAuthToken             = cfgFumAuth
                , integrCfgFumBaseUrl               = cfgFumBaseUrl
                , integrCfgFumEmployeeListName      = cfgFumUserList
                -- GitHub
                , integrCfgGithubProxyBaseRequest   = cfgGhBaseReq
                , integrCfgGithubOrgName            = cfgGhOrg
                -- Flowdock
                , integrCfgFlowdockToken            = cfgFdAuth
                , integrCfgFlowdockOrgName          = cfgFdOrg
                }

        -- Contacts action
        let getContacts = runIntegrations cfg contacts

        -- Action returning the contact list
        let action = cachedIO cache 3600 () getContacts

        -- Periodically try to fetch new data
        _ <- spawnPeriocron (Options logger 300)
            [ (Job "update contacts" action, every 300)
            ]
        pure action
