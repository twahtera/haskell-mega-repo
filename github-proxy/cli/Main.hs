{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
module Main (main) where

import Prelude ()
import Futurice.Prelude
import Data.Constraint
import Futurice.EnvConfig            (parseEnvVar)
import Futurice.Has                  (FlipIn)
import Futurice.Integrations.Classes (MonadGitHub (..))
import Futurice.Integrations.GitHub  (GHR (..), initDataSource)
import Network.HTTP.Client
       (Manager, Request, applyBasicAuth, newManager, parseUrlThrow)
import Network.HTTP.Client.TLS       (tlsManagerSettings)

import qualified Futurice.GitHub as GH
import qualified Haxl.Core       as H

main :: IO ()
main = do
    -- config
    baseUrl  <- parseEnvVar "GITHUBPROXY_CLI_ENDPOINT"
    authUser <- parseEnvVar "GITHUBPROXY_CLI_HTTPUSER"
    authPass <- parseEnvVar "GITHUBPROXY_CLI_HTTPPASS"
    -- assemble
    baseReq <- parseUrlThrow baseUrl
    let baseReq' = applyBasicAuth authUser authPass baseReq
    -- http manager
    manager <- newManager tlsManagerSettings
    -- execute
    result <- runH manager baseReq' script0
    -- print
    print result

script0 :: MonadGitHub m => m GH.Organization
script0 = githubReq $ GH.publicOrganizationR "futurice"

-------------------------------------------------------------------------------
-- H(axl) Monad
-------------------------------------------------------------------------------

newtype H a = H { unH :: H.GenHaxl () a }

instance Functor H where
    fmap f (H x) = H (fmap f x)

instance Applicative H where
    pure = H . pure
    H f <*> H x = H (f <*> x)
    H f *> H x = H (f *> x)

instance Monad H where
    return = pure
    (>>) = (*>)
    H f >>= k = H $ f >>= unH . k

instance MonadGitHub H where
    type MonadGitHubC H = FlipIn GH.GHTypes
    githubReq req = case (showDict, typeableDict) of
        (Dict, Dict) -> H (H.dataFetch $ GHR tag req)
      where
        tag = GH.mkTag
        showDict     = GH.tagDict (Proxy :: Proxy Show) tag
        typeableDict = GH.tagDict (Proxy :: Proxy Typeable) tag

runH :: Manager -> Request -> H a -> IO a
runH mgr req (H haxl) = withStderrLogger $ \lgr -> do
    let stateStore = H.stateSet (initDataSource lgr mgr req) H.stateEmpty
    env <- H.initEnv stateStore ()
    H.runHaxl env haxl
