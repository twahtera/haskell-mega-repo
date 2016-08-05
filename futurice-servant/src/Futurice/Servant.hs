{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.Servant (
    -- * @main@ boilerplate
    futuriceServerMain,
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
    -- * Lower-level
    -- ** Server API
    FuturiceAPI,
    futuriceServer,
    -- ** WAI
    Application,
    -- ** Cache
    DynMapCache,
    newDynMapCache,
    cachedIO,
    ) where

-- TOOD: add middleware

import Futurice.Prelude
import Prelude ()

import Control.Concurrent.STM   (atomically)
import Data.Swagger
import Development.GitRev       (gitCommitDate, gitHash)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), SColour)
import Network.Wai              (Application)
import Servant
import Servant.Cache.Class      (DynMapCache, cachedIO)
import Servant.Futurice.Favicon (FutuFaviconAPI, serveFutuFavicon)
import Servant.Futurice.Status  hiding (info)
import Servant.HTML.Lucid       (HTML)
import System.IO              (stderr)
import Servant.Swagger
import Servant.Swagger.UI

import qualified Data.Text.IO                  as T
import qualified Servant.Cache.Internal.DynMap as DynMap
import qualified Network.Wai.Handler.Warp      as Warp

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
-- main boilerplate
-------------------------------------------------------------------------------

futuriceServerMain
    :: forall cfg ctx api proxy proxy' colour.
       (HasSwagger api,  HasServer api '[], SColour colour)
    => Text                 -- ^ Service name
    -> Text                 -- ^ Service description
    -> proxy colour
    -> IO cfg               -- ^ Read config
    -> (cfg -> Int)         -- ^ Get port from the config
    -> proxy' api
    -> (ctx -> Server api)  -- ^ Application
    -> (cfg -> DynMapCache -> IO ctx)
       -- ^ Initialise the context for application
    -> IO ()
futuriceServerMain t d _proxyColour getConfig cfgPort _proxyApi server makeCtx = do
    T.hPutStrLn stderr $ "Hello, " <> t <> " is alive"
    cfg         <- getConfig
    let p       = cfgPort cfg
    cache       <- newDynMapCache
    ctx         <- makeCtx cfg cache
    let server' = futuriceServer t d cache proxyApi (server ctx)
                :: Server (FuturiceAPI api colour)
    T.hPutStrLn stderr $ "Starting " <> t <> " at port " <> show p ^. packed
    Warp.run p (serve proxyApi' server')
  where
    proxyApi :: Proxy api
    proxyApi = Proxy

    proxyApi' :: Proxy (FuturiceAPI api colour)
    proxyApi' = Proxy

-------------------------------------------------------------------------------
-- Other stuff
-------------------------------------------------------------------------------

newDynMapCache :: IO DynMapCache
newDynMapCache = DynMap.newIO
