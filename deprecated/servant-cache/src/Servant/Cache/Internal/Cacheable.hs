{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Cache.Internal.Cacheable
-- Copyright   :  (C) 2015 Futurice Oy
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Servant.Cache.Internal.Cacheable where

import Control.Monad.Trans.Except
import Data.Hashable                (Hashable)
import Data.Proxy                   (Proxy (..))
import Data.Tagged                  (Tagged (..), untag)
import Data.Typeable                (Typeable)
import GHC.TypeLits                 (KnownSymbol, symbolVal)
import Servant.API
import Servant.API.ContentTypes     (AllCTRender)
import Servant.Cache.Internal.HList
import Servant.Server

type ServerM = ExceptT ServantErr IO

------------------------------------------------------------------------------
-- Cacheable typeclass
------------------------------------------------------------------------------

class ( Eq (CacheKey layout)
      , Hashable (CacheKey layout)
      , Typeable (CacheKey layout)
      , Eq (PList (CacheParams layout))
      , PHList (CacheParams layout)
      , Hashable (PList (CacheParams layout))
      , Typeable (PList (CacheParams layout))
      , Typeable (CacheResult layout)
      , HCurry (Server layout) (CacheParams layout) (ServerM (CacheResult layout))
      )
   => Cacheable layout where
   type CacheKey layout :: *
   type CacheParams layout :: [*]
   type CacheResult layout :: *
   cacheKey :: Tagged layout (CacheKey layout)

instance (AllCTRender ctypes a, Typeable a) => Cacheable (Get ctypes a) where
   type CacheKey (Get ctypes a) = ()
   type CacheParams (Get ctypes a) = '[]
   type CacheResult (Get ctypes a) = a
   cacheKey = Tagged ()

instance (KnownSymbol path, Cacheable sublayout)
      => Cacheable (path :> sublayout) where
    type CacheKey (path :> sublayout) = (String, CacheKey sublayout)
    type CacheParams (path :> sublayout) = CacheParams sublayout
    type CacheResult (path :> sublayout) = CacheResult sublayout

    cacheKey = Tagged (symbolVal pathProxy, untag subCacheKey)
      where subCacheKey = cacheKey :: Tagged sublayout (CacheKey sublayout)
            pathProxy = Proxy :: Proxy path

instance (KnownSymbol capture, FromHttpApiData a, Cacheable sublayout 
         , Eq a, Hashable a, Typeable a
         )
     => Cacheable (Capture capture a :> sublayout) where
    type CacheKey (Capture capture a :> sublayout) = CacheKey sublayout
    type CacheParams (Capture capture a :> sublayout) =
        a ': CacheParams sublayout
    type CacheResult (Capture capture a :> sublayout) = CacheResult sublayout

    cacheKey = Tagged $ untag subCacheKey
      where subCacheKey = cacheKey :: Tagged sublayout (CacheKey sublayout)
