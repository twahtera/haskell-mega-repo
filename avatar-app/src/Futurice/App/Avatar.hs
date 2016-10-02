{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Codec.Picture              (DynamicImage)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Futurice.Servant
import Servant
import System.IO                  (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE

import Network.HTTP.Client
       (Manager, httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Avatar modules
import Futurice.App.Avatar.API
import Futurice.App.Avatar.Config (Config (..), getConfig)
import Futurice.App.Avatar.Logic  (avatar)

type Ctx = (DynMapCache, Manager)

type DynamicImage' = Headers '[Header "Cache-Control" Text] DynamicImage

mkAvatar
    :: Ctx
    -> Maybe Text  -- ^ URL, is mandatory
    -> Maybe Int   -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> ExceptT ServantErr IO DynamicImage'
mkAvatar _ Nothing _ _ =
    throwE $ ServantErr 400 errMsg (fromString errMsg) []
  where
    errMsg = "'url' query parameter is required"
mkAvatar (cache, mgr) (Just url) msize grey = ExceptT . fmap (first f) $ do
    hPutStrLn stderr $ mconcat
        [ "fetching ", T.unpack url
        , " size: ", show msize
        , " grey: ", show grey
        ]
    req <- parseUrlThrow (T.unpack url)
    -- XXX: The cache will eventually fill if service is abused
    res <- cachedIO cache 3600 url $ httpLbs req mgr
    (fmap . fmap) (addHeader "public, max-age=3600")
        . cachedIO cache 3600 (url, size, grey)
        . pure . avatar size grey . responseBody
        $ res
  where
    size  = max 16 $ fromMaybe 32 msize
    f err = ServantErr
        500
        "Avatar conversion error"
        (LBS.fromStrict . TE.encodeUtf8 . T.pack $ err)
        []

server :: Ctx -> Server AvatarAPI
server ctx = pure "Hello from avatar app"
    :<|> mkAvatar ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName          .~ "Avatar API"
    & serverDescription   .~ "Serve smaller versions of your favourite images"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverGetConfig     .~ getConfig
    & serverApp avatarApi .~ server
  where
    makeCtx :: Config -> DynMapCache -> IO Ctx
    makeCtx _cfg cache = do
        mgr <- newManager tlsManagerSettings
        return (cache, mgr)
