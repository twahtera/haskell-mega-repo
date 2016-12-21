{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.Servant (
    -- * @main@ boilerplate
    futuriceServerMain,
    futuriceNoMiddleware,
    liftFuturiceMiddleware,
    -- * HTML (lucid)
    HTML,
    -- * CSV (cassava)
    CSV,
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
    -- * SSO user
    SSOUser,
    -- * Lower-level
    -- ** Server API
    FuturiceAPI,
    futuriceServer,
    ServerConfig,
    emptyServerConfig,
    serverName,
    serverDescription,
    serverApp,
    serverMiddleware,
    serverColour,
    serverEnvPfx,
    -- ** WAI
    Application,
    Middleware,
    -- ** Cache
    DynMapCache,
    newDynMapCache,
    cachedIO,
    genCachedIO,
    CachePolicy(..),
    -- * Middlewares
    logStdoutDev,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM               (atomically)
import Control.Lens                         (Lens, LensLike)
import Control.Monad.Catch                  (fromException, handleAll)
import Data.Char                            (isAlpha)
import Data.Swagger                         hiding (port)
import Development.GitRev                   (gitCommitDate, gitHash)
import Futurice.Cache
       (CachePolicy (..), DynMapCache, cachedIO, genCachedIO)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), SColour)
import Futurice.EnvConfig                   (Configure, getConfigWithPorts)
import GHC.Prim                             (coerce)
import Log.Backend.Logentries               (withLogentriesLogger)
import Network.Wai
       (Middleware, requestHeaders, responseLBS)
import Network.Wai.Metrics                  (metrics, registerWaiMetrics)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.CSV.Cassava                  (CSV)
import Servant.Futurice.Favicon             (FutuFaviconAPI, serveFutuFavicon)
import Servant.Futurice.Status              hiding (info)
import Servant.HTML.Lucid                   (HTML)
import Servant.Server.Internal              (passToServer)
import Servant.Swagger
import Servant.Swagger.UI
import System.Remote.Monitoring             (forkServer, serverMetricStore)

import qualified Data.Aeson               as Aeson
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified FUM
import qualified Futurice.DynMap          as DynMap
import qualified Network.HTTP.Types       as H
import qualified Network.Wai.Handler.Warp as Warp

type FuturiceAPI api colour =
    FutuFaviconAPI colour
    :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"
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
    :: forall api colour. (HasSwagger api)
    => Text  -- ^ title
    -> Text  -- ^ description
    -> DynMapCache
    -> Proxy api
    -> Server api
    -> Server (FuturiceAPI api colour)
futuriceServer t d cache papi server
    = serveFutuFavicon
    :<|> swaggerSchemaUIServer (swaggerDoc t d papi)
    :<|> serveStatus (stats cache)
    :<|> server

-------------------------------------------------------------------------------
-- main boilerplate
-------------------------------------------------------------------------------

-- | Data type containing the server setup
data ServerConfig f (colour :: Colour) ctx api = SC
    { _serverName        :: !Text
    , _serverDescription :: !Text
    , _serverApplication :: ctx -> Server api
    , _serverMiddleware  :: ctx -> Middleware
    , _serverEnvPfx      :: !(f Text)
    }

-- | Default server config, through the lenses the type of api will be refined
--
emptyServerConfig :: ServerConfig Proxy 'FutuGreen ctx (Get '[JSON] ())
emptyServerConfig = SC
    { _serverName         = "Futurice Service"
    , _serverDescription  = "Some futurice service"
    , _serverApplication  = \_ -> pure ()
    , _serverMiddleware   = futuriceNoMiddleware
    , _serverEnvPfx       = Proxy
    }

-- | Default middleware: i.e. nothing.
futuriceNoMiddleware :: ctx -> Middleware
futuriceNoMiddleware = liftFuturiceMiddleware id

-- | Lift config-less middleware for use with 'futuriceServerMain'.
liftFuturiceMiddleware :: Middleware -> ctx -> Middleware
liftFuturiceMiddleware mw _ = mw

serverName :: Lens' (ServerConfig f colour ctx api) Text
serverName = lens _serverName $ \sc x -> sc { _serverName = x }

serverDescription :: Lens' (ServerConfig f colour ctx api) Text
serverDescription = lens _serverDescription $ \sc x -> sc { _serverDescription = x }

serverEnvPfx :: Lens
    (ServerConfig f colour ctx api)
    (ServerConfig I colour ctx api)
    (f Text)
    Text
serverEnvPfx = lens _serverEnvPfx $ \sc x -> sc { _serverEnvPfx = I x }

serverApp
    :: Functor f
    => Proxy api'
    -> LensLike f (ServerConfig g colour ctx api) (ServerConfig g colour ctx api')
       (ctx -> Server api) (ctx -> Server api')
serverApp _ = lens _serverApplication $ \sc x -> sc { _serverApplication = x }

serverMiddleware :: Lens' (ServerConfig g colour ctx api) (ctx -> Middleware)
serverMiddleware = lens _serverMiddleware $ \sc x -> sc { _serverMiddleware = x }

serverColour
    :: Lens (ServerConfig f colour ctx api) (ServerConfig f colour' ctx api)
       (Proxy colour) (Proxy colour')
serverColour = lens (const Proxy) $ \sc _ -> coerce sc

futuriceServerMain
    :: forall cfg ctx api colour.
       (Configure cfg, HasSwagger api, HasServer api '[], SColour colour)
    => (cfg -> Logger -> DynMapCache -> IO ctx)
       -- ^ Initialise the context for application
    -> ServerConfig I colour ctx api
       -- ^ Server configuration
    -> IO ()
futuriceServerMain makeCtx (SC t d server middleware (I envpfx)) =
    withStderrLogger $ \logger ->
    handleAll (handler logger) $ do
        runLogT "futurice-servant" logger $ logInfo_ $ "Hello, " <> t <> " is alive"
        (cfg, p, ekgP, leToken) <- getConfigWithPorts logger (envpfx ^. from packed)
        cache          <- newDynMapCache

        withLogentriesLogger leToken $ \leLogger -> do
            let logger' = logger <> leLogger
            ctx            <- makeCtx cfg logger' cache
            let server'    =  futuriceServer t d cache proxyApi (server ctx)
                           :: Server (FuturiceAPI api colour)

            store      <- serverMetricStore <$> forkServer "localhost" ekgP
            waiMetrics <- registerWaiMetrics store


            runLogT "futurice-servant" logger' $ do
                logInfo_ $ "Starting " <> t <> " at port " <> textShow p
                logInfo_ $ "-          http://localhost:" <> textShow p <> "/"
                logInfo_ $ "- swagger: http://localhost:" <> textShow p <> "/swagger-ui/"
                logInfo_ $ "- ekg:     http://localhost:" <> textShow ekgP <> "/"

            Warp.runSettings (settings p logger')
                $ metrics waiMetrics
                $ middleware ctx
                $ serve proxyApi' server'
  where
    handler logger e = do
        runLogT "futurice-servant" logger $ logAttention_ $ textShow e
        throwM e

    settings p logger = Warp.defaultSettings
        & Warp.setPort p
        & Warp.setOnException (onException logger)
        & Warp.setOnExceptionResponse onExceptionResponse
        & Warp.setServerName (TE.encodeUtf8 t)

    onException logger mreq e = do
        runLogT "warp" logger $ do
            logAttention (textShow e) mreq

    -- On exception return JSON
    -- TODO: we could return some UUID and log exception with it.
    -- but maybe it's worth doing only when errors are rare.
    onExceptionResponse e = responseLBS
        s
        [(H.hContentType, "application/json; charset=utf-8")]
        (Aeson.encode ("Something went wrong" :: Text))
      where
        s = case fromException e :: Maybe Warp.InvalidRequest of
            Just _  -> H.badRequest400
            Nothing -> H.internalServerError500

    proxyApi :: Proxy api
    proxyApi = Proxy

    proxyApi' :: Proxy (FuturiceAPI api colour)
    proxyApi' = Proxy

-------------------------------------------------------------------------------
-- Other stuff
-------------------------------------------------------------------------------

newDynMapCache :: IO DynMapCache
newDynMapCache = DynMap.newIO

-------------------------------------------------------------------------------
-- SSO User
-------------------------------------------------------------------------------

data SSOUser

instance HasServer api context => HasServer (SSOUser :> api) context where
    type ServerT (SSOUser :> api) m = Maybe FUM.UserName -> ServerT api m

    route Proxy context subserver =
        route (Proxy :: Proxy api) context (passToServer subserver ssoUser)
      where
        ssoUser req = FUM.UserName . T.filter isAlpha . TE.decodeLatin1 <$>
            lookup "HTTP_REMOTE_USER" (requestHeaders req)

instance HasLink api => HasLink (SSOUser :> api) where
    type MkLink (SSOUser :> api) = MkLink api
    toLink _ = toLink (Proxy :: Proxy api)

instance HasSwagger api => HasSwagger (SSOUser :> api) where
    toSwagger _ = toSwagger (Proxy :: Proxy api)
