{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import Futurice.Prelude
import Prelude ()

import           Control.Monad.PlanMill
                 (MonadPlanMillConstraint (..), MonadPlanMillQuery (..))
import           Data.Constraint
import           Futurice.Constraint.Unit1               (Unit1)
import           Futurice.EnvConfig                      (parseDefaultPort)
import qualified Haxl.Core                               as H
import           Network.HTTP.Client
                 (Manager, Request, newManager, parseUrl)
import           Network.HTTP.Client.TLS                 (tlsManagerSettings)
import qualified PlanMill                                as PM
import qualified PlanMill.Queries                        as Q
import           PlanMill.Queries.Haxl                   (initDataSourceBatch)
import qualified PlanMill.Types.Query                    as Q
import           Text.PrettyPrint.ANSI.Leijen.AnsiPretty
                 (AnsiPretty (..), linebreak, putDoc)

main :: IO ()
main = do
    port <- parseDefaultPort "PLANMILLPROXY"
    let baseUrl = "http://localhost:" ++ show port ++ "/haxl"
    baseReq <- parseUrl baseUrl
    manager <- newManager tlsManagerSettings
    result <- runH manager baseReq script0
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
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) (Sub Dict) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     (Sub Dict) q

runH :: Manager -> Request -> H a -> IO a
runH mgr req (H haxl) = do
    let stateStore = H.stateSet (initDataSourceBatch mgr req) H.stateEmpty
    env <- H.initEnv stateStore ()
    H.runHaxl env haxl
