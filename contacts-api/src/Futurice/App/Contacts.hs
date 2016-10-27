{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Contacts (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Data.Unique        (newUnique)
import Futurice.Periocron
import Futurice.Servant
import Servant

-- Contacts modules
import Futurice.App.Contacts.API
import Futurice.App.Contacts.Config   (Config (..))
import Futurice.App.Contacts.Executor (execute)
import Futurice.App.Contacts.Logic    (contacts)
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
  where
    makeCtx :: Config -> DynMapCache -> IO Ctx
    makeCtx Config {..} cache = do
        let getContacts = execute contacts
                cfgGhOrg
                cfgFdOrg
                cfgFumUserList
                cfgFumAuth
                cfgFumBaseUrl
                cfgFdAuth
                cfgGhAuth
        unique <- newUnique

        -- Action returning the contact list
        let action = cachedIO cache 3600 unique getContacts

        -- Periodically try to fetch new data
        _ <- spawnPeriocron (Options runStderrLoggingT 300)
            [ (Job "update contacts" action, every 300)
            ]
        pure action
