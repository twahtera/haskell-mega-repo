{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Contacts (defaultMain) where

import Prelude        ()
import Prelude.Compat

import Control.Monad.IO.Class
import Data.Text              (Text)
import Data.Unique            (newUnique)
import Network.Wai
import Servant
import System.IO              (hPutStrLn, stderr)
import Futurice.Servant

import qualified Network.Wai.Handler.Warp      as Warp

-- Contacts modules
import Futurice.App.Contacts.API
import Futurice.App.Contacts.Config (Config (..), getConfig)
import Futurice.App.Contacts.Logic  (contacts)
import Futurice.App.Contacts.Executor  (execute)
import Futurice.App.Contacts.Types

-- | API server
server :: IO [Contact Text] -> Server ContactsAPI
server action = liftIO action :<|> liftIO action

-- | Server with docs and cache and status
server' :: DynMapCache -> IO [Contact Text] -> Server ContactsAPI'
server' cache cs = futuriceServer
    "Contacts API"
    "All employees and externals"
    cache contactsAPI (server cs)

-- | Wai application
app :: DynMapCache -> IO [Contact Text] -> Application
app cache cs = serve contactsAPI' (server' cache cs)

-- TODO: add periocron

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, I'm alive"
    Config{..} <- getConfig
    let getContacts = execute contacts
            cfgGhOrg
            cfgFdOrg
            cfgFumUserList
            cfgFumAuth
            cfgFumBaseUrl
            cfgFdAuth
            cfgGhAuth
    cache <- newDynMapCache 
    unique <- newUnique
    let getContacts' = cachedIO cache 3600 unique getContacts
    let app' = app cache getContacts'
    hPutStrLn stderr "Starting web service"
    Warp.run cfgPort app'
