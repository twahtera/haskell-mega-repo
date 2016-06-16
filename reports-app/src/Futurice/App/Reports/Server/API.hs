{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Reports.Server.API where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM (atomically)
import Futurice.Colour

import qualified Data.Text                     as T
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified Servant.HTML.Lucid            as Lucid

import Servant
import Servant.Cache.Class      (DynMapCache)
import Servant.Futurice.Favicon
import Servant.Futurice.Status

import Futurice.App.Reports.Types

type ReportsAPI = Get '[Lucid.HTML] IndexPage
    :<|> "issues" :> Get '[Lucid.HTML, JSON] IssueReport
    :<|> "fum-github" :> Get '[Lucid.HTML, JSON] FumGitHubReport

type ReportsAPI' = ReportsAPI :<|> AuxAPI ('FutuAccent 'AF2 'AC3)

avatarApi :: Proxy ReportsAPI
avatarApi = Proxy

avatarApi' :: Proxy ReportsAPI'
avatarApi' = Proxy

serverAvatarApi :: DynMapCache -> String -> Server ReportsAPI -> Server ReportsAPI'
serverAvatarApi cache now server = server :<|> serverAuxApi cache now avatarApi

-- Utils

type AuxAPI c =
    FutuFaviconAPI c
    :<|> StatusAPI

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

serverAuxApi
    :: (SColour c)
     => DynMapCache
     -> String            -- ^ gitrev hash
     -> Proxy api
     -> Server (AuxAPI c)
serverAuxApi cache versionHash _ =
    serveFutuFavicon
    :<|> serveStatus (stats cache versionHash)
