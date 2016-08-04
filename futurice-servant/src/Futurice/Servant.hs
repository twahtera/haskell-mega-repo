{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.Servant (
    -- * Server API
    FuturiceAPI,
    futuriceServer,
    -- * Cache
    DynMapCache,
    newDynMapCache,
    cachedIO,
    -- * HTML (lucid)
    HTML,
    -- * Swagger
    -- | These are useful for defining empty schemas
    --
    -- @
    -- instance 'ToSchema' IndexPage where
    --     declareNamedSchema _ = pure $ 'NamedSchema' (Just "Indexpage") mempty
    -- @
    --
    NamedSchema (..), ToSchema (..), ToParamSchema (..),
    -- * Favicon
    Colour (..),
    AccentColour (..),
    AccentFamily (..),
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Concurrent.STM   (atomically)
import Data.Swagger
import Development.GitRev       (gitCommitDate, gitHash)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), SColour)
import Servant
import Servant.Cache.Class      (DynMapCache, cachedIO)
import Servant.Futurice.Favicon (FutuFaviconAPI, serveFutuFavicon)
import Servant.Futurice.Status  hiding (info)
import Servant.HTML.Lucid       (HTML)
import Servant.Swagger
import Servant.Swagger.UI

import qualified Servant.Cache.Internal.DynMap as DynMap

type SwaggerSchemaEndpoint = "swagger.json" :> Get '[JSON] Swagger

type FuturiceAPI api colour =
    FutuFaviconAPI colour
    :<|> SwaggerSchemaEndpoint
    :<|> SwaggerUI "swagger-ui" SwaggerSchemaEndpoint (SwaggerSchemaEndpoint :<|> api)
    :<|> StatusAPI
    :<|> api

stats :: DynMapCache -> StatusInfoIO
stats dmap = gcStatusInfo <> dynmapStats
  where
    dynmapStats :: StatusInfoIO
    dynmapStats = SIIO $ group "cache" . metric "size" <$> dynmapSize

    dynmapSize :: IO Int
    dynmapSize = atomically $ DynMap.size dmap

swaggerDoc
    :: HasSwagger api
    => Text  -- ^ title
    -> Text  -- ^ description
    -> Proxy api
    -> Swagger
swaggerDoc t d proxy = toSwagger proxy
    & info.title       .~ t
    & info.version     .~ fromString v
    & info.description ?~ d
  where
    v = $(gitCommitDate) ++ " " ++ $(gitHash)

-- | Create futurice server
futuriceServer
    :: forall api colour. (HasSwagger api, SColour colour)
    => Text  -- ^ title
    -> Text  -- ^ description
    -> DynMapCache
    -> Proxy api
    -> Server api
    -> Server (FuturiceAPI api colour)
futuriceServer t d cache papi server
    = serveFutuFavicon
    :<|> return (swaggerDoc t d papi)
    :<|> swaggerUIServer
    :<|> serveStatus (stats cache)
    :<|> server

-------------------------------------------------------------------------------
-- Other stuff
-------------------------------------------------------------------------------

newDynMapCache :: IO DynMapCache
newDynMapCache = DynMap.newIO
