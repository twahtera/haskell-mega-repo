{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Avatar (defaultMain) where

import Prelude          ()
import Futurice.Prelude 

import Codec.Picture              (DynamicImage)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Time                  (getCurrentTime)
import Development.GitRev         (gitHash)
import Network.Wai
import Servant
import Servant.Cache.Class        (DynMapCache, cachedIO)
import System.IO                  (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Servant.Cache.Internal.DynMap as DynMap

import Network.HTTP.Client     (Manager, httpLbs, newManager, parseUrl,
                                responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Avatar modules
import Futurice.App.Avatar.API
import Futurice.App.Avatar.Config  (Config (..), getConfig)
import Futurice.App.Avatar.Logic   (avatar)
import Futurice.App.Avatar.Orphans ()

-- | /TODO:/ We probably will have some
type Ctx = (DynMapCache, Manager)

type DynamicImage' = Headers '[Header "Cache-Control" Text] DynamicImage

mkAvatar :: Ctx
         -> Maybe Text  -- ^ URL, is mandatory
         -> Maybe Int   -- ^ size, minimum size is 16
         -> Bool        -- ^ greyscale
         -> ExceptT ServantErr IO DynamicImage'
mkAvatar _ Nothing _ _ =
    throwE $ ServantErr 400 errMsg (fromString errMsg) []
  where
    errMsg = "'url' query parameter is required"
mkAvatar (cache, mgr) (Just url) msize grey = ExceptT . fmap (first f) $ do
    hPutStrLn stderr $
        mconcat [ "fetching ", T.unpack url
                , " size: ", show msize
                , " grey: ", show grey
                ]
    req <- parseUrl (T.unpack url)
    -- XXX: The cache will eventually fill is service is abused
    res <- cachedIO cache 3600 url $ httpLbs req mgr
    (fmap . fmap) (addHeader "public, max-age=3600")
        . cachedIO cache 3600 (url, size, grey)
        . pure . avatar size grey . responseBody
        $ res
  where
    size  = max 16 $ fromMaybe 32 msize
    f err = ServantErr 500
                       "Avatar conversion error"
                       (LBS.fromStrict . TE.encodeUtf8 . T.pack $ err)
                       []

-- | API server
server :: Ctx -> Server AvatarAPI
server ctx = pure "Hello from avatar app"
    :<|> mkAvatar ctx

-- | Server with docs and cache and status
server' :: DynMapCache -> UTCTime -> String -> Ctx -> Server AvatarAPI'
server' cache start hash' ctx = serverAvatarApi cache start hash' (server ctx)

-- | Wai application
app :: DynMapCache -> UTCTime -> String -> Ctx -> Application
app cache start hash' ctx = serve avatarApi' (server' cache  start hash' ctx)

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, avatar-app is alive"
    Config{..} <- getConfig
    mgr <- newManager tlsManagerSettings
    cache <- DynMap.newIO
    now <- getCurrentTime
    let ctx = (cache, mgr)
    let app' = app cache now $(gitHash) ctx
    hPutStrLn stderr "Starting web server"
    Warp.run cfgPort app'
