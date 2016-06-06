{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Contacts.API where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM (atomically)
import Data.Time              (diffUTCTime, getCurrentTime)
import Development.GitRev

import qualified Data.Text                     as T
import qualified Servant.Cache.Internal.DynMap as DynMap

import Futurice.Colour

import Servant
import Servant.Cache
import Servant.Cache.Class      (DynMapCache)
import Servant.Docs
import Servant.Futurice.Favicon
import Servant.Futurice.Status

import Futurice.App.Contacts.Types

type ContactsAPI =
    Get '[JSON] [Contact T.Text]
    :<|> "contacts.json" :> Get '[JSON] [Contact T.Text]

type DocsAPI = "docs.md" :> Get '[PlainText] T.Text

type DataAPI = "version" :> Get '[PlainText] T.Text
    :<|> "uptime" :> Get '[PlainText] T.Text

type ContactsAPI' = Cached SomeCache 60 :> ContactsAPI
    :<|> DocsAPI
    :<|> FutuFaviconAPI ('FutuAccent 'AF2 'AC3)
    :<|> StatusAPI
    :<|> DataAPI

serveDocs :: HasDocs api => Proxy api -> Server DocsAPI
serveDocs api = pure docsText
  where
    docsText :: T.Text
    docsText = T.pack
             . markdown
             $ docsWithIntros [intro] api
    intro = DocIntro "Welcome" ["This is our contacts API.", "Enjoy!"]

stats :: DynMapCache -> StatusInfoIO
stats dmap = gcStatusInfo <> dynmapStats
  where
    dynmapStats :: StatusInfoIO
    dynmapStats = SIIO $ group "cache" . metric "size" <$> dynmapSize

    dynmapSize :: IO Int
    dynmapSize = atomically $ DynMap.size dmap

serverWithDocs :: forall api c ttl. (HasDocs api, SColour c)
               => DynMapCache
               -> UTCTime
               -> Proxy api
               -> Server api
               -> Server (Cached SomeCache ttl :> api :<|> DocsAPI :<|> FutuFaviconAPI c :<|> StatusAPI :<|> DataAPI)
serverWithDocs cache startTime p server =
    server
        :<|> serveDocs p
        :<|> serveFutuFavicon
        :<|> serveStatus (stats cache)
        :<|> (pure (T.pack $(gitHash)) :<|> liftIO uptime)
  where
    uptime = do
        now <- getCurrentTime
        let diff = now `diffUTCTime` startTime
        pure $ textShow diff

contactsAPI :: Proxy ContactsAPI
contactsAPI = Proxy

contactsAPI' :: Proxy ContactsAPI'
contactsAPI' = Proxy
