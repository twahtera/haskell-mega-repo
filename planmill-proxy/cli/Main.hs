{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
module Main (main) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.PlanMill
       (MonadPlanMillConstraint (..), MonadPlanMillQuery (..))
import Data.Constraint
import Futurice.Constraint.Unit1 (Unit1)
import Futurice.EnvConfig
import Network.HTTP.Client
       (Manager, Request, applyBasicAuth, newManager, parseUrlThrow)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import PlanMill.Queries.Haxl     (initDataSourceBatch)

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty
       (AnsiPretty (..), linebreak, putDoc)

import qualified Haxl.Core            as H
import qualified PlanMill             as PM
import qualified PlanMill.Queries     as Q
import qualified PlanMill.Types.Query as Q

main :: IO ()
main = withStderrLogger $ \logger -> do
    -- config
    (baseUrl, authUser, authPass) <- getConfig' logger "PLANMILLPROXY" $ (,,)
        <$> envVar "ENDPOINT"
        <*> envVar "HTTPUSER"
        <*> envVar "HTTPPASS"
    -- assemble
    baseReq <- parseUrlThrow baseUrl
    let baseReq' = applyBasicAuth authUser authPass baseReq
    -- http manager
    manager <- newManager tlsManagerSettings
    -- execute
    result <- runH manager logger baseReq' script0
    -- print
    putDoc . (<> linebreak) . ansiPretty $ result

script0 :: MonadPlanMillQuery m => m PM.Me
script0 = Q.me

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

instance MonadPlanMillConstraint H where
    type MonadPlanMillC H = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance MonadPlanMillQuery H where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> H (H.dataFetch q)
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

runH :: Manager -> Logger -> Request -> H a -> IO a
runH mgr lgr req (H haxl) = do
    let stateStore = H.stateSet (initDataSourceBatch lgr mgr req) H.stateEmpty
    env <- H.initEnv stateStore ()
    H.runHaxl env haxl
