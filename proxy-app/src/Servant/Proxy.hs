{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Servant.Proxy (
    makeProxy,
    ProxyPair,
    ProxyServer,
    -- * Internal
    Convertible,
    ) where

import Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import Futurice.Prelude
import Prelude ()
import Servant
import Servant.Client

-- | Definition of a proxy.
--
-- * @public@: public endpoint
--
-- * @service@: service to connect to
--
-- * @private@: service endpoint
--
data ProxyPair public service private

-- | Extract servant api for public part of the proxy.
type family ProxyServer def where
    ProxyServer '[p]      = ProxyServer' p
    ProxyServer (p ': ps) = ProxyServer' p :<|> ProxyServer ps

type family ProxyServer' def where
    ProxyServer' (ProxyPair public _s _p) = public

-- | Class to convert client functions to server functions
class Convertible client server | client -> server, server -> client where
    convert :: ClientEnv -> client -> server

instance Convertible (ClientM a) (Handler a) where
    convert env cli = withExceptT transformError $ ExceptT $ runClientM cli env
      where
        transformError err = err504 { errBody = fromString $ show err }

-- | Class describing proxable endpoints
--
-- 'ClientEnv' paramater is taken tagged, so we don't mix them.
makeProxy
    :: forall public service private.
       ( HasClient private
       , Convertible (Client private) (ServerT public Handler)
       )
    => Proxy (ProxyPair public service private) -> Tagged service ClientEnv -> Server public
makeProxy _ (Tagged env) = convert env (client proxyPrivate)
  where
    proxyPrivate = Proxy :: Proxy private
