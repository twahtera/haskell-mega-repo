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
import Data.Time              (UTCTime, getCurrentTime)
import Data.Unique            (newUnique)
import Network.Wai
import Servant
import Servant.Cache          (SomeCache (..))
import Servant.Cache.Class    (DynMapCache, cachedIO)
import System.IO              (hPutStrLn, stderr)

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Servant.Cache.Internal.DynMap as DynMap

-- Contacts modules
import Futurice.App.Contacts.API
import Futurice.App.Contacts.Config (Config (..), getConfig)
import Futurice.App.Contacts.Logic  (contacts)
import Futurice.App.Contacts.Executor  (execute)
import Futurice.App.Contacts.Types

import Futurice.App.Contacts.Orphans ()

-- | API server
server :: IO [Contact Text] -> Server ContactsAPI
server action = liftIO action :<|> liftIO action

-- | Server with docs and cache and status
server' :: DynMapCache -> UTCTime -> IO [Contact Text] -> Server ContactsAPI'
server' cache startTime cs =
    serverWithDocs cache startTime contactsAPI (server cs)

-- | Wai application
app :: DynMapCache -> UTCTime -> IO [Contact Text] -> Application
app cache startTime cs =
    serveWithContext  contactsAPI' context (server' cache startTime cs)
  where
    context = SomeCache cache :. EmptyContext

-- TODO: add periocron
-- TODO: add swagger

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, I'm alive"
    now <- getCurrentTime
    Config{..} <- getConfig
    let getContacts = execute contacts
            cfgGhOrg
            cfgFdOrg
            cfgFumUserList
            cfgFumAuth
            cfgFumBaseUrl
            cfgFdAuth
            cfgGhAuth
    cache <- DynMap.newIO
    unique <- newUnique
    let getContacts' = cachedIO cache 3600 unique getContacts
    let app' = app cache now getContacts'
    hPutStrLn stderr "Starting web service"
    Warp.run cfgPort app'
