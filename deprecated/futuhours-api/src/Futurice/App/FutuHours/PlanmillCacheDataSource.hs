{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.FutuHours.PlanmillCacheDataSource
    ( PlanmillCacheRequest
    , initDataSource
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.Async (async, wait)
import Control.Monad.Catch      (handle)
import Control.Monad.Http       (HttpT, evalHttpT)
import Data.Binary.Tagged       (taggedDecodeOrFail, taggedEncode)
import Data.BinaryFromJSON      (BinaryFromJSON)
import Futurice.CryptoRandom
       (CRandT, CryptoGenError, CryptoGen, evalCRandTThrow, mkCryptoGen)
import Futurice.Has
import Haxl.Core
import Haxl.Typed

import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple.Fxtra as Postgres

import Futurice.App.FutuHours.Context

import qualified PlanMill      as PM
import qualified PlanMill.Eval as PM

import Futurice.App.FutuHours.PlanMillCache

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

data PlanmillCacheRequest a where
    Timereports :: PM.Interval Day -> PM.UserId -> PlanmillCacheRequest PM.Timereports

deriving instance Show (PlanmillCacheRequest a)
deriving instance Typeable PlanmillCacheRequest
deriving instance Eq (PlanmillCacheRequest a)

instance Haxl.Core.Show1 PlanmillCacheRequest where show1 = show

instance Hashable (PlanmillCacheRequest a) where
    hashWithSalt salt (Timereports interval uid) = salt
        `hashWithSalt` (0 :: Int)
        `hashWithSalt` interval
        `hashWithSalt` uid

instance StateKey PlanmillCacheRequest where
    data State PlanmillCacheRequest = PMRS Postgres.Connection PM.Cfg Logger

instance DataSourceName PlanmillCacheRequest where
    dataSourceName _ = "PlanmillDataSource"

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

initDataSource
    :: forall env. (HasDevelopment env, HasPlanmillCfg env, HasLogger env)
    => env
    -> Postgres.Connection
    -> IO (State PlanmillCacheRequest)
initDataSource e conn = pure $
    PMRS conn (e ^. planmillCfg) (e ^. logger)

instance In' PlanmillCacheRequest r => MonadPlanMillCached (GenTyHaxl r u) where
    cachedTimereports interval uid = GenTyHaxl . dataFetch $
        Timereports interval uid

-------------------------------------------------------------------------------
-- Fetching
-------------------------------------------------------------------------------

type M = CRandT CryptoGen CryptoGenError :$ ReaderT PM.Cfg :$ LogT :$ HttpT IO

-- | TODO: implement local cache
instance DataSource u PlanmillCacheRequest where
    fetch (PMRS conn cfg lgr) _flags _userEnv blockedFetches =
        AsyncFetch $ \inner -> do
            a <- async action
            inner
            wait a
      where
        action = do
            g <- mkCryptoGen
            evalHttpT
                . runLogT "planmill-cached-data-source" lgr
                . flip runReaderT cfg
                . flip evalCRandTThrow g
                $ traverse_ singleFetch blockedFetches

        singleFetch :: BlockedFetch PlanmillCacheRequest -> M ()
        singleFetch (BlockedFetch (Timereports interval uid) v) = do
            let interval' = PM.ResultInterval PM.IntervalStart $ PM.intervalDayToIntervalUTC interval
            res <- singleFetch' $ PM.timereportsFromIntervalFor interval' uid
            liftIO $ putSuccess v res

        singleFetch' :: forall a. BinaryFromJSON a => PM.PlanMill a -> M a
        singleFetch' req =  do
            r <- runMaybeT $ do
                logTrace_ $ "Looking in Postgres cache: " <> url
                r <- MaybeT $ liftIO $ Postgres.singleQuery conn selectQuery (Postgres.Only url)
                extract url r
            maybe evaledPlanMill return r
          where
            url' :: Text
            url' = T.pack $ PM.fromUrlParts (PM.requestUrlParts req)

            qs' :: Text
            qs' = textShow $ case req of
                PM.PlanMillGet qs _      -> qs
                PM.PlanMillPagedGet qs _ -> qs
                PM.PlanMillPost _ _      -> mempty

            url :: Text
            url = url' <> qs'

            evaledPlanMill :: M a
            evaledPlanMill = do
                logTrace_ $ "Requesting API: " <> url
                x <- PM.evalPlanMill req
                -- Store in postgres
                let bs = taggedEncode x
                handle omitSqlError $ liftIO $ void' $ Postgres.query conn
                    "SELECT futuhours.upsert_cache (?, ?)"
                    (url, Postgres.Binary bs)
                pure x

            void' :: Functor m => m [Postgres.Only ()] -> m ()
            void' x = () <$ x

selectQuery :: Postgres.Query
selectQuery = "SELECT data FROM futuhours.cache WHERE path = ? and updated + interval '200 minutes' * (r + 1) > current_timestamp;"

-- | Extract cache value from single db query result
extract
    :: (Alternative m, MonadLog m, BinaryFromJSON a)
    => Text
    -> Postgres.Only (Postgres.Binary BSL.ByteString)
    -> m a
extract url (Postgres.Only (Postgres.Binary bs)) = case taggedDecodeOrFail bs of
    Right (bsLeft, _, x)
        | BSL.null bsLeft -> return x
        | otherwise       -> do
            logTrace_ $ "Didn't consume all input from cache: " <> url
            empty
    Left (_, _, err) -> do
            logAttention_ $ "Cannot decode cached value: " <> url <> " -- " <> T.pack err
            empty

omitSqlError :: MonadLog m => Postgres.SqlError -> m ()
omitSqlError err = do
    logAttention_ $ textShow err
    return ()
