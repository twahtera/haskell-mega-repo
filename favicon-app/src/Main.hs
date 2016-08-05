{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Futurice.Prelude
import Prelude ()

import Codec.Picture       (Image, PixelRGBA8)
import Futurice.Colour
import Futurice.Logo
import Futurice.Servant
import Lucid               hiding (for_)
import Servant
import Servant.JuicyPixels (PNG)
import System.Environment  (lookupEnv)

type IconAPI = "icon" :> Capture "colour" Colour :> Get '[PNG] (Image PixelRGBA8)

type API =
    Get '[HTML] IndexPage
    :<|> IconAPI

api :: Proxy API
api = Proxy

iconEndpoint :: Proxy IconAPI
iconEndpoint = Proxy

server :: DynMapCache -> Server API
server cache = pure IndexPage :<|> liftIO . makeLogo'
 where
   makeLogo' c = cachedIO cache 3600 c (evaluate $!! makeLogo c)

main :: IO ()
main = futuriceServerMain
    "Favicon API"
    "Futurice favicons"
    (Proxy :: Proxy 'FutuBlack)
    getConfig cfgPort
    api server
    $ \_cfg -> return

getConfig :: IO Int 
getConfig = fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"

cfgPort :: Int -> Int
cfgPort = id

-------------------------------------------------------------------------------
-- IndexPage
-------------------------------------------------------------------------------

data IndexPage = IndexPage

instance ToHtml IndexPage where
    toHtmlRaw = toHtml
    toHtml _ = doctypehtml_ $ for_ [minBound..maxBound] $ \colour ->
        let link = textShow $ safeLink api iconEndpoint colour
        in a_ [href_ link] $ img_ [src_ link]

instance ToSchema IndexPage where
    declareNamedSchema _ = pure $ NamedSchema (Just "Indexpage") mempty

-- /TODO/: move to prelude
instance ToSchema (Image a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Image") mempty

-- /TODO/: move to prelude
instance ToParamSchema Colour where
    toParamSchema = mempty
