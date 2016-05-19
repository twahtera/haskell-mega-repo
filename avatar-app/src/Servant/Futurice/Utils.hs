{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Futurice.Utils (
    -- * APIs
    DocsAPI,
    AuxAPI,
    -- * server
    serverAuxApi,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Concurrent.STM (atomically)
import Control.Lens
import Data.Semigroup         ((<>))
import Data.Time              (UTCTime)
import Futurice.Colour
import Text.Blaze.Html        (Html)

import qualified Data.Text                     as T
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified Servant.HTML.Blaze            as Blaze
import qualified Text.Markdown                 as Markdown

import Servant
import Servant.Cache.Class      (DynMapCache)
import Servant.Docs             (DocIntro (..), ExtraInfo, HasDocs,
                                 defaultDocOptions, docsWith, markdown)
import Servant.Futurice.Favicon
import Servant.Futurice.Status

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
    intro = DocIntro "Welcome" ["This is Avatar API.", "Enjoy!"]

stats :: DynMapCache -> UTCTime -> String -> StatusInfoIO
stats dmap start hash =
    gcStatusInfo <> dynmapStats <> uptimeStatusInfo start <> versionStats
  where
    dynmapStats :: StatusInfoIO
    dynmapStats = SIIO $ group "cache" . metric "size" <$> dynmapSize

    dynmapSize :: IO Int
    dynmapSize = atomically $ DynMap.size dmap

    versionStats :: StatusInfoIO
    versionStats = infoIO "version" (pure $ T.pack hash)

serverAuxApi :: (HasDocs api, SColour c)
             => DynMapCache
             -> UTCTime
             -> String            -- ^ gitrev hash
             -> Proxy api
             -> ExtraInfo api
             -> Server (AuxAPI c)
serverAuxApi cache start hash p extra =
         serveDocs p extra
    :<|> serveFutuFavicon
    :<|> serveStatus (stats cache start hash)
