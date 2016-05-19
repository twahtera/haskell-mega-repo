{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.Proxy (
    Proxied,
    Proxyable(..),
    Proxyable'(..),
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import Servant
import Servant.Swagger
import Servant.API.ContentTypes
import Servant.Client
import GHC.TypeLits               (KnownSymbol, Symbol)

data Proxied (api :: k)

class
    ( Proxyable' (ProxiedAPI api)
    , KnownSymbol (ProxyNamespace api)
    )
  => Proxyable (api :: k) where
    type ProxyNamespace api :: Symbol
    type ProxiedAPI api :: *

-- | Helper class for @api = 'ProxiedAPI' api'@.
--
-- We have to define helper type families @C@ and @S@, because @HasServer@ has
-- overlapping instances.
--
-- We have to add more instances when we want to proxy more sophisticated APIs.
class Proxyable' api where
    type C api :: *
    type C api = Client api
    type S api :: *
    type S api = Server api

    proxy' :: Proxy api -> C api -> S api

instance (KnownSymbol sym, Proxyable' api) => Proxyable' (sym :> api) where
    type C (sym :> api) = C api
    type S (sym :> api) = S api
    proxy' _ = proxy' (Proxy :: Proxy api)

instance (MimeUnrender ct a, AllCTRender (ct ': cts) a) => Proxyable' (Get (ct ': cts) a) where
    type C (Get (ct ': cts) a) = ExceptT ServantError IO a
    type S (Get (ct ': cts) a) = ExceptT ServantErr IO a
    proxy' _ cli = withExceptT f cli 
      where
        f err = err504 { errBody = fromString $ show err }

-- | Not sure if there is the way to wrap @'Server' api@ into @'Delayed' ('Server' api)@.
-- Then we could make @ServerT (Proxied api) m = ()@ which would be much nicer.
instance (Proxyable api, HasServer (ProxiedAPI api) context)
  => HasServer (Proxied api) context where
    type ServerT (Proxied api) m = ServerT (ProxyNamespace api :> ProxiedAPI api) m

    route _p = route p
      where
        p = Proxy :: Proxy (ProxyNamespace api :> ProxiedAPI api)

instance (Proxyable api, HasSwagger (ProxiedAPI api))
  => HasSwagger (Proxied api) where
    toSwagger _ = toSwagger (Proxy :: Proxy (ProxyNamespace api :> ProxiedAPI api))
