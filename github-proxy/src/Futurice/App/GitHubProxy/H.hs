{-# LANGUAGE CPP          #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
module Futurice.App.GitHubProxy.H (
    H, runH,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Operational     (Program, interpretWithMonad, singleton)
import Futurice.Constraint.Unit1     (Unit1)
import Futurice.Integrations.Classes (MonadGitHub (..))
import GitHub.Auth                   (Auth)

import qualified GitHub as GH

newtype H a = H { unH :: Program (GH.Request 'GH.RA) a }

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
    type MonadGitHubC H = Unit1
    githubReq req = H (singleton req)

runH :: Auth -> H a -> IO a
runH auth (H m) = do
    mgr <- newManager tlsManagerSettings
    interpretWithMonad (interpret mgr) m
  where
    interpret :: Manager -> GH.Request 'GH.RA x -> IO x
    interpret mgr req = do 
        GH.executeRequestWithMgr mgr auth req >>= either throwM pure
