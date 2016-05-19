{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Cache.Internal.HasCachedServer
-- Copyright   :  (C) 2015 Futurice Oy
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Servant.Cache.Internal.HasCachedServer where

import Prelude        ()
import Prelude.Compat

import Data.Hashable            (Hashable)
import Data.Tagged              (Tagged (..), untag)
import Data.Time                (NominalDiffTime)
import Data.Typeable            (Typeable)
import GHC.TypeLits             (KnownSymbol)
import Servant                  hiding (HList (..))
import Servant.API.ContentTypes (AllCTRender)

import           Servant.Cache.Class              (Cache, SP (..))
import qualified Servant.Cache.Class              as Cache
import           Servant.Cache.Internal.Cacheable
import           Servant.Cache.Internal.HList

class HasCachedServer layout where
   cachedServer :: Cache c
                => c -> Proxy layout -> NominalDiffTime
                -> Server layout -> Server layout

instance (AllCTRender ctypes a, Typeable a)
       => HasCachedServer (Get ctypes a) where
    cachedServer = cachedServer'

instance (KnownSymbol path, Cacheable sublayout)
       => HasCachedServer (path :> sublayout) where
    cachedServer = cachedServer'

instance (KnownSymbol capture, FromHttpApiData a, Cacheable sublayout
         , Eq a, Hashable a, Typeable a
         )
      => HasCachedServer (Capture capture a :> sublayout) where
    cachedServer = cachedServer'

instance (HasCachedServer a, HasCachedServer b)
      => HasCachedServer (a :<|> b) where
    cachedServer cache _proxy ttl (a :<|> b) =
        (cachedServer cache proxyA ttl a :<|> cachedServer cache proxyB ttl b)
      where
        proxyA :: Proxy a
        proxyA = Proxy
        proxyB :: Proxy b
        proxyB = Proxy

cachedServer' :: forall layout cache. (Cacheable layout, Cache cache)
             => cache -> Proxy layout -> NominalDiffTime
             -> Server layout -> Server layout
cachedServer' cache _proxy ttl server = server'
  where
    hserver :: HList (CacheParams layout) -> ServerM (CacheResult layout)
    hserver = hUncurry Proxy server

    hserver' :: HList (CacheParams layout) -> ServerM (CacheResult layout)
    hserver' params = do
        let mapkey = (SP key $ toPList params)
        Cache.cached cache ttl mapkey (hserver params)

    server' :: Server layout
    server' = hCurry Proxy hserver'

    key :: CacheKey layout
    key = untag (cacheKey :: Tagged layout (CacheKey layout))
