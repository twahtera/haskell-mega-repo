{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.GitHubDashboard.Server.API where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM (atomically)
import Futurice.Colour
import Text.Blaze.Html        (Html)

import qualified Data.Text                     as T
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified Servant.HTML.Blaze            as Blaze
import qualified Servant.HTML.Lucid            as Lucid
import qualified Text.Markdown                 as Markdown

import Servant
import Servant.Cache.Class      (DynMapCache)
import Servant.Docs             (DocIntro (..), ExtraInfo, HasDocs,
                                 defaultDocOptions, docsWith, markdown)
import Servant.Futurice.Favicon
import Servant.Futurice.Status

import Futurice.App.GitHubDashboard.Types

type DashboardAPI = Get '[PlainText] Text
    :<|> "pullrequests.json" :> Get '[JSON] PullRequests
    :<|> "issues" :> Get '[Lucid.HTML, JSON] Issues 

type DashboardAPI' = DashboardAPI :<|> AuxAPI ('FutuAccent 'AF5 'AC2)

avatarApi :: Proxy DashboardAPI 
avatarApi = Proxy

avatarApi' :: Proxy DashboardAPI'
avatarApi' = Proxy

serverAvatarApi :: DynMapCache -> String -> Server DashboardAPI -> Server DashboardAPI'
serverAvatarApi cache now server = server :<|> serverAuxApi cache now avatarApi mempty

-- Utils

-- TODO: remove docs
-- TODO: add ekg
type DocsAPI =
         "docs.md"   :> Get '[PlainText] T.Text
    :<|> "docs.html" :> Get '[Blaze.HTML] Html

type AuxAPI c =
         DocsAPI
    :<|> FutuFaviconAPI c
    :<|> StatusAPI

serveDocs :: HasDocs api => Proxy api -> ExtraInfo api -> Server DocsAPI
serveDocs api extra = pure docsMd :<|> pure docsHtml
  where
    docsHtml = Markdown.markdown Markdown.def (docsMd ^. lazy)
    docsMd = T.pack docs

    docs :: String
    docs = markdown $ docsWith defaultDocOptions [intro] extra api

    intro :: DocIntro
    intro = DocIntro "Welcome" ["This is dashboard API.", "Enjoy!"]

stats :: DynMapCache -> String -> StatusInfoIO
stats dmap versionHash =
    gcStatusInfo <> dynmapStats <> versionStats
  where
    dynmapStats :: StatusInfoIO
    dynmapStats = SIIO $ group "cache" . metric "size" <$> dynmapSize

    dynmapSize :: IO Int
    dynmapSize = atomically $ DynMap.size dmap

    versionStats :: StatusInfoIO
    versionStats = infoIO "version" (pure $ T.pack versionHash)

serverAuxApi :: (HasDocs api, SColour c)
             => DynMapCache
             -> String            -- ^ gitrev hash
             -> Proxy api
             -> ExtraInfo api
             -> Server (AuxAPI c)
serverAuxApi cache versionHash p extra =
         serveDocs p extra
    :<|> serveFutuFavicon
    :<|> serveStatus (stats cache versionHash)
