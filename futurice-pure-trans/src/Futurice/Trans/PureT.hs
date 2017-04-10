{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.Trans.PureT (
    -- * Transformer
    PureT (..),
    evalPureT,
    -- * Class
    MonadPureBase (..),
    -- * Instances
    -- ** Log
    HasLogger (..),
    HasLoggerEnv (..),
    LoggerEnv,
    mkLoggerEnv,
    -- ** Http
    H.HasHttpManager (..),
    -- ** Crypto
    CryptoPool, 
    HasCryptoPool (..),
    -- ** PlanMill
    PM.HasPlanMillCfg (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.CryptoRandom

import Control.Concurrent.MVar.Lifted (MVar, modifyMVar)
import Control.Lens                   (ASetter', to)
import Control.Monad.CryptoRandom     (ContainsGenError, GenError)
import Control.Monad.Http             (MonadHttp (..))
import Data.Aeson                     (FromJSON, Value (Object), object)
import Data.Constraint
import Data.Pool                      (Pool, withResource)
import Data.TDigest.Metrics           (MonadMetrics (..))
import Log
       (LogMessage (..), LoggerEnv (..), MonadLog (..), execLogger)
import PlanMill.Eval                  (evalPlanMill)

import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as H
import qualified PlanMill            as PM

-------------------------------------------------------------------------------
-- Transformer
-------------------------------------------------------------------------------

-- | A 'Control.Monad.Trans.Reader.ReaderT' copy, with different instances.
newtype PureT e r m a = PureT { runPureT :: r -> m a }

evalPureT :: r -> PureT e r m a -> m a
evalPureT = flip runPureT

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Inverse of 'MonadBase'.
class MonadBase b m => MonadPureBase b m | m -> b where
    -- | Lower computation to the base monad.
    lowerBase :: m a -> m (b a)

instance MonadPureBase IO IO where
    lowerBase = pure

instance MonadPureBase b m => MonadPureBase b (PureT e r m) where
    lowerBase (PureT m) = PureT $ \r -> lowerBase (m r)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor m => Functor (PureT e r m) where
    fmap f (PureT m) = PureT (fmap f . m)
    {-# INLINE fmap #-}

    x <$ PureT m = PureT $ \r -> x <$ m r
    {-# INLINE (<$) #-}

instance Applicative m => Applicative (PureT e r m) where
    pure = PureT . const . pure
    {-# INLINE pure #-}

    PureT f <*> PureT x = PureT $ \r -> f r <*> x r
    {-# INLINE (<*>) #-}

    PureT a *> PureT b = PureT $ \r -> a r *> b r
    {-# INLINE (*>) #-}

    PureT a <* PureT b = PureT $ \r -> a r <* b r
    {-# INLINE (<*) #-}

instance Monad m => Monad (PureT e r m) where
    return = pure
    {-# INLINE return #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

    PureT m >>= k = PureT $ \r -> m r >>= \a -> runPureT (k a) r
    {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (PureT e r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadTrans (PureT e r) where
    lift = PureT . const
    {-# INLINE lift #-}

instance MonadBase b m => MonadBase b (PureT e r m) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance Monad m => MonadReader r (PureT e r m) where
    ask = PureT pure
    {-# INLINE ask #-}

    reader f = PureT $ pure . f

    local f (PureT m) = PureT $ \r -> m (f r)

instance MonadTransControl (PureT e r) where
    type StT (PureT e r) a = a
    liftWith f = PureT $ \r -> f $ \t -> runPureT t r
    restoreT = PureT . const
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (PureT e r m) where
    type StM (PureT e r m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance MonadThrow m => MonadThrow (PureT e r m) where
    throwM = lift . throwM

-- | With this instance, you cannot catch anything.
instance (MonadThrow m, ContainsGenError e) => MonadError e (PureT e r m) where
    throwError _ = throwM PureTAbortException
    catchError m _ = m

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

localOver :: MonadReader r m => ASetter' r a -> (a -> a) -> m x -> m x
localOver l f = local (over l f)

data PureTAbortException = PureTAbortException
  deriving (Show, Typeable)

instance Exception PureTAbortException

-------------------------------------------------------------------------------
-- Log
-------------------------------------------------------------------------------

class HasLogger a where
    logger :: Lens' a Logger

class HasLoggerEnv a where
    loggerEnv :: Lens' a LoggerEnv

    loggerEnvData :: Lens' a [AesonPair]
    loggerEnvData = loggerEnv . lens leData (\e x -> e { leData = x })

    loggerEnvDomain :: Lens' a [Text]
    loggerEnvDomain = loggerEnv . lens leDomain (\e x -> e { leDomain = x })

    loggerEnvLogger :: Lens' a Logger
    loggerEnvLogger = loggerEnv . lens leLogger (\e x -> e { leLogger = x })

mkLoggerEnv :: Text -> Logger -> LoggerEnv
mkLoggerEnv comp lgr = LoggerEnv
    { leLogger    = lgr
    , leComponent = comp
    , leDomain    = []
    , leData      = []
    }

instance (MonadBase IO m, MonadTime m, HasLoggerEnv r) => MonadLog (PureT e r m) where
    logMessage time level message data_ = do
        LoggerEnv {..} <- view loggerEnv
        liftBase $ do
            lm <- evaluate $!! LogMessage
                { lmComponent = leComponent
                , lmDomain    = leDomain
                , lmTime      = time
                , lmLevel     = level
                , lmMessage   = message
                , lmData      = case data_ of
                    Object obj      -> Object . (<>) obj $ HM.fromList leData
                    _ | null leData -> data_
                      | otherwise   -> object $ ("_data", data_) : leData
                }
            execLogger leLogger lm

    localData data_ = localOver loggerEnvData (data_ ++)
    localDomain domain = localOver loggerEnvDomain (++ [domain])

-------------------------------------------------------------------------------
-- Http
-------------------------------------------------------------------------------

instance (H.HasHttpManager r, MonadBase IO m) => MonadHttp (PureT e r m) where
    httpLbs req = do
        mgr <- view (to H.getHttpManager)
        liftBase $ H.httpLbs req mgr

-------------------------------------------------------------------------------
-- Crypto
-------------------------------------------------------------------------------

type CryptoPool = Pool (MVar CryptoGen)

class HasCryptoPool a where
    cryptoPool :: Lens' a CryptoPool

embedCRandT
    :: (MonadBaseControl IO m, ContainsGenError e, MonadThrow m, HasCryptoPool r)
    => CRandT CryptoGen GenError m a -> PureT e r m a
embedCRandT action = PureT $ \r ->
    withResource (r ^. cryptoPool) $ \mvar ->
        modifyMVar mvar $ \g -> do
            x <- runCRandT action g
            case x of
                -- should we re-init?
                Left _        -> throwM PureTAbortException
                Right (a, g') -> pure (g', a)

instance (MonadBaseControl IO m, ContainsCryptoGenError e, MonadThrow m, HasCryptoPool r)
    => MonadCRandom e (PureT e r m)
  where
    getCRandom                 = embedCRandT getCRandom
    getBytes n                 = embedCRandT (getBytes n)
    getBytesWithEntropy n entr = embedCRandT (getBytesWithEntropy n entr)
    doReseed seed              = embedCRandT (doReseed seed)

-------------------------------------------------------------------------------
-- PlanMill
-------------------------------------------------------------------------------

instance Monad m => PM.MonadPlanMillConstraint (PureT e r m) where
    type MonadPlanMillC (PureT e r m) = FromJSON
    entailMonadPlanMillCVector _ _    = Sub Dict

instance
    ( Monad m, MonadIO m, MonadBaseControl IO m
    , MonadThrow m
    , MonadTime m, MonadClock m, MonadMetrics m
    , ContainsGenError e, H.HasHttpManager r, HasCryptoPool r, HasLoggerEnv r
    , PM.HasPlanMillCfg r
    )
    => PM.MonadPlanMill (PureT e r m)
  where
    planmillAction = evalPlanMill
