{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Servant.Futurice.Favicon (
    FutuFaviconAPI,
    futuFaviconAPI,
    FutuFavicon,
    serveFutuFavicon,
    ) where

import Prelude        ()
import Prelude.Compat

import Codec.Picture       (encodePng)
import Data.Proxy          (Proxy (..))
import Data.Tagged         (Tagged (..), untag)
import Data.Typeable       (Typeable)
import Futurice.Colour     (Colour, SColour (..))
import Futurice.Logo       (makeLogo)
import Servant.API
import Servant.JuicyPixels (PNG)
import Servant.Server

type FutuFaviconAPI (c :: Colour) = "favicon.ico" :> Get '[PNG] (FutuFavicon c)

futuFaviconAPI :: Proxy FutuFavicon
futuFaviconAPI = Proxy

data FutuFavicon (c :: Colour) = FutuFavicon
    deriving (Typeable)

instance SColour c => MimeRender PNG (FutuFavicon c) where
    mimeRender _ _ = encodePng (makeLogo c)
      where c = untag (scolour :: Tagged c Colour)

serveFutuFavicon :: SColour c => Server (FutuFaviconAPI c)
serveFutuFavicon = pure FutuFavicon

