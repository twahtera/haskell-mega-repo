{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeOperators             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Cache
-- Copyright   :  (C) 2015 Futurice Oy
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Cached servant api endpoints.
--
-- @
-- type CachedUserAPI = Cached 60 ("users" :> Get '[JSON] [User])
-- @
module Servant.Cache (
    -- * API combinators
    Cached,
    -- * Cache
    Cache,
    SomeCache(..),
    mkCache,
   ) where

import Prelude        ()
import Prelude.Compat

import Data.Time              (NominalDiffTime)
import Data.Typeable          (Typeable)
import GHC.TypeLits           (KnownNat, Nat, natVal)

import Servant      hiding (HList (..))
import Servant.Docs (HasDocs (..))

import qualified Servant.Cache.Internal.DynMap    as DynMap
import           Servant.Cache.Internal.HasCachedServer
import           Servant.Cache.Class

-- | Cached end-point.
--
-- @
-- type API = Cached 3600 :> Get '[JSON] Model
-- @
--
-- Currently the cached result is always returned, even when it's expired based on
-- time-to-live.  In the next version of the library this will be configurable
data Cached c (ttl :: Nat)
    deriving Typeable

instance
    ( HasServer layout context
    , HasCachedServer layout
    , KnownNat ttl
    , Cache c, HasContextEntry context c
    )
  => HasServer (Cached c ttl :> layout) context where
    type ServerT (Cached c ttl :> layout) m = ServerT layout m

    route _proxy context server =
        route proxy context (fmap (cachedServer cache proxy ttl) server)
      where
        proxy :: Proxy layout
        proxy = Proxy

        cache :: c
        cache = getContextEntry context

        ttl :: NominalDiffTime
        ttl = fromInteger $ natVal (Proxy :: Proxy ttl)

------------------------------------------------------------------------------
-- Non altered functionality
------------------------------------------------------------------------------

instance HasDocs layout => HasDocs (Cached c ttl :> layout) where
    docsFor _proxy = docsFor (Proxy :: Proxy layout)

type instance IsElem' e (Cached c ttl :> s) = IsElem e s

-- TODO: client

------------------------------------------------------------------------------
-- Cache
------------------------------------------------------------------------------

-- | Create cache from DynMap
mkCache :: IO SomeCache
mkCache = SomeCache <$> DynMap.newIO
