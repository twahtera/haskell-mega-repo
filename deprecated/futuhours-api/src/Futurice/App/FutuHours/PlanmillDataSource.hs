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
module Futurice.App.FutuHours.PlanmillDataSource
    ( PlanmillRequest
    , initDataSource
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.Async         (async, wait)
import Control.Monad.Catch              (handle)
import Control.Monad.Http               (HttpT, evalHttpT)
import Futurice.CryptoRandom
       (CRandT, CryptoGenError, CryptoGen, evalCRandTThrow, mkCryptoGen)
import Data.Binary.Tagged               (taggedDecodeOrFail, taggedEncode)
import Data.BinaryFromJSON              (BinaryFromJSON)
import Data.Constraint                  (Dict (..), type (:-)(..))
import Futurice.Has
import Haxl.Core
import Haxl.Typed

import qualified Data.ByteString.Lazy             as BSL
import qualified Data.HashMap.Strict              as HM
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple.Fxtra as Postgres

import Futurice.App.FutuHours.Context

import qualified PlanMill      as PM
import qualified PlanMill.Eval as PM

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

data PlanmillRequest a where
    PMR :: BinaryFromJSON a => PM.PlanMill a -> PlanmillRequest a

deriving instance Show (PlanmillRequest a)
deriving instance Typeable PlanmillRequest
deriving instance Eq (PlanmillRequest a)

instance Haxl.Core.Show1 PlanmillRequest where show1 = show

instance Hashable (PlanmillRequest a) where
    hashWithSalt salt (PMR r) = hashWithSalt salt r

instance StateKey PlanmillRequest where
    data State PlanmillRequest = PMRS Postgres.Connection PM.Cfg Logger

instance DataSourceName PlanmillRequest where
    dataSourceName _ = "PlanmillDataSource"

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

initDataSource
    :: forall env. (HasDevelopment env, HasPlanmillCfg env, HasLogger env)
    => env
    -> Postgres.Connection
    -> IO (State PlanmillRequest)
initDataSource e conn = pure $
    PMRS conn (e ^. planmillCfg) (e ^. logger)

instance In' PlanmillRequest r => PM.MonadPlanMillConstraint (GenTyHaxl r u) where
    type MonadPlanMillC (GenTyHaxl r u) = BinaryFromJSON
    entailMonadPlanMillCVector _ _ = Sub Dict

instance In' PlanmillRequest r => PM.MonadPlanMill (GenTyHaxl r u) where
    planmillAction = GenTyHaxl . dataFetch . PMR

-------------------------------------------------------------------------------
-- Fetching
-------------------------------------------------------------------------------

type M = CRandT CryptoGen CryptoGenError :$ ReaderT PM.Cfg :$ LogT :$ HttpT IO

-- | Postgres cache key, i.e. path
newtype Key = Key { getKey :: Text }
    deriving (Eq, Ord, Show)

instance Postgres.ToField Key where
    toField = Postgres.toField . getKey

instance Postgres.FromField Key where
    fromField f mbs = Key <$> Postgres.fromField f mbs

instance Hashable Key where
    hashWithSalt salt = hashWithSalt salt . getKey

-- | Blocked fetch together with it's key
data P = P
    { pKey  :: !Key
    , _pReq :: !(BlockedFetch PlanmillRequest)
    }

makeP :: BlockedFetch PlanmillRequest -> P
makeP bf@(BlockedFetch (PMR req) _) = P key bf
  where
    key :: Key
    key = Key $ url <> qs

    url :: Text
    url = T.pack $ PM.fromUrlParts (PM.requestUrlParts req)

    qs :: Text
    qs = textShow $ case req of
        PM.PlanMillGet qs' _      -> qs'
        PM.PlanMillPagedGet qs' _ -> qs'
        PM.PlanMillPost _ _       -> mempty

-- | Results from DB
type CacheLookup = HashMap Key BSL.ByteString

makeCacheLookup :: [(Key, Postgres.Binary BSL.ByteString)] -> CacheLookup
makeCacheLookup = HM.fromList . (fmap . fmap) Postgres.fromBinary

instance DataSource u PlanmillRequest where
    fetch (PMRS conn cfg lgr) _flags _userEnv blockedFetches =
        AsyncFetch $ \inner -> do
            a <- async action
            inner
            wait a
      where
        action = do
            -- Precalculate keys for each fetch
            let blockedFetches' = makeP <$> blockedFetches
            -- Ask all at once from DB
            cache <- makeCacheLookup <$> Postgres.query conn selectQuery (Postgres.Only . Postgres.In $ pKey <$> blockedFetches')
            -- Make rg
            g <- mkCryptoGen
            -- Go thru each request individual, using postgres results
            evalHttpT
                . runLogT "planmill-data-source" lgr
                . flip runReaderT cfg
                . flip evalCRandTThrow g
                $ do logInfo_ $ "Blocked fetches " <> textShow (HM.size cache) <> " / " <> textShow (length blockedFetches')
                     traverse_ (singleFetch cache) blockedFetches'

        singleFetch :: CacheLookup -> P -> M ()
        singleFetch cache (P key (BlockedFetch (PMR req) v)) = do
            -- Lookup in cache
            inCache <- join <$> extract key `traverse` HM.lookup key cache
            -- If not found perform fetch
            res <- maybe (singleFetch' key req) pure inCache
            -- Return
            liftIO $ putSuccess v res

        -- | Perform actual fetch and store in DB
        singleFetch' :: forall a. BinaryFromJSON a => Key -> PM.PlanMill a -> M a
        singleFetch' key req = do
            logTrace_ $ "Requesting API: " <> getKey key
            x <- PM.evalPlanMill req
            -- Store in postgres
            let bs = taggedEncode x
            handle omitSqlError $ liftIO $ void' $ Postgres.query conn
                "SELECT futuhours.upsert_cache (?, ?)"
                (key, Postgres.Binary bs)
            pure x

        void' :: Functor m => m [Postgres.Only ()] -> m ()
        void' x = () <$ x

selectQuery :: Postgres.Query
selectQuery = "SELECT path, data FROM futuhours.cache WHERE path in ? and updated + interval '200 minutes' * (r + 1) > current_timestamp;"

-- | Extract cache value from single db query result
extract
    :: forall m a. (MonadLog m, BinaryFromJSON a)
    => Key
    -> BSL.ByteString
    -> m (Maybe a)
extract key bs = case taggedDecodeOrFail bs of
    Right (bsLeft, _, x)
        | BSL.null bsLeft -> return $ Just x
        | otherwise       -> do
            logAttention_ $ "Didn't consume all input from cache: " <> getKey key
            return Nothing
    Left (_, _, err) -> do
            logAttention_ $ "Cannot decode cached value: " <> getKey key <> " -- " <> T.pack err
            return Nothing

omitSqlError :: MonadLog m => Postgres.SqlError -> m ()
omitSqlError err = do
    logAttention_ $ textShow err
    return ()
