{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import Futurice.Prelude
import Prelude ()

import Codec.Picture       (Image, PixelRGBA8)
import Control.Lens        (iso)
import Futurice.Colour
import Futurice.EnvConfig  (HasPort (..))
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

newtype Config = Config { getPort :: Int }
type Ctx = DynMapCache

server :: Ctx -> Server API
server cache = pure IndexPage :<|> liftIO . makeLogo'
 where
   makeLogo' c = cachedIO cache 3600 c (evaluate $!! makeLogo c)

main :: IO ()
main = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName        .~ "Favicon API"
    & serverDescription .~ "Futurice favicons"
    & serverColour      .~ (Proxy :: Proxy 'FutuBlack)
    & serverGetConfig   .~ getConfig
    & serverApp api     .~ server
  where
    makeCtx :: Config -> DynMapCache -> IO Ctx
    makeCtx _cfg = return

getConfig :: IO Config
getConfig = Config . fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"

instance HasPort Config where
    port = iso getPort Config

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
