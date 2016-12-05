{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Cache.Class (
    Cache(..),
    SomeCache(..),
    DynMapCache,
    SP(..),
    Leaf(..),
    I(..),
    cached,
    cachedIO,
    genCachedIO,
    CachePolicy (..),
    ) where

import Prelude ()
import Futurice.Prelude            hiding (lookup)
import Control.Concurrent.Async    (Async, async, wait, waitCatchSTM)
import Control.Concurrent.STM      (STM, atomically)
import Data.Time                   (addUTCTime, getCurrentTime)

import qualified Servant.Cache.Internal.DynMap as DynMap

-- | Strict pair.
data SP a b = SP !a !b
    deriving (Eq, Ord, Show, Read, Functor, Typeable, Generic)

instance (Hashable a, Hashable b) => Hashable (SP a b)

data Leaf v = Leaf !UTCTime !(Async v)
    deriving (Functor, Typeable, Generic)

class Cache c where
    -- TODO: change order of parameters
    lookup :: (Typeable k, Eq k, Hashable k, Typeable v)
           => k -> Proxy v -> c -> STM (Maybe (Leaf v))
    lookup k _ c = lookup' k c (SP Nothing)

    -- TODO: change order of parameters
    insert :: (Typeable k, Eq k, Hashable k, Typeable v)
           => k -> Leaf v -> c -> STM ()

    lookup' :: (Typeable k, Eq k, Hashable k, Typeable v)
            => k  -- ^ Key
            -> c  -- ^ Cache
            -> (Maybe (Leaf v) -> (SP (Maybe UTCTime) a))
                  -- ^ Strategy. Can be used to update stored value timestamp
            -> STM a
    lookup' k c f = do
        l <- lookup k p c
        case f l of
            (SP Nothing r) -> pure r
            (SP (Just newStamp) r) ->
                case l of
                     Nothing         -> pure r
                     Just (Leaf _ v) -> r <$ insert k (Leaf newStamp v) c
      where
        p :: Proxy v
        p = Proxy

    {-# MINIMAL (lookup' | lookup), insert #-}

-- | Cached action.
--
-- Simple example:
--
-- @
-- c <- mkCache
-- cached c 10 'k' (putStrLn "foo" >> pure 'v')
-- @
cached :: forall c k v b m.
          ( Cache c
          , Typeable k, Eq k, Hashable k
          , Typeable v
          , MonadBaseControl b m, b ~ IO
          )
       => c                -- ^ Cache
       -> NominalDiffTime  -- ^ Time-to-live
       -> k                -- ^ Cache key
       -> m v              -- ^ Value action
       -> m v
cached cache ttl key action = do
    now <- liftBase getCurrentTime
    let expirationMoment = ttl `addUTCTime` now
    ro <- liftBase $ atomically $ lookup' key cache $ \r ->
        case r of
            Nothing -> SP Nothing Nothing
            Just (Leaf stamp r')
                -- If data in cache and recent enough, use it
                | stamp > now ->
                     SP Nothing (Just $ SP r' False)
                -- otherwise, re-insert it with new timestamp so concurrent
                -- requests will still reuse old value.  But return
                -- nothing, so this request will be handled by the actual
                -- application.
                | otherwise -> do
                     SP (Just expirationMoment) (Just $ SP r' True)

    --  Action, which updates the cache
    let cachedAction :: m v
        cachedAction = do
            r' <- action :: m v
            r'' <- liftBase $ async $ pure r' :: m (Async v)
            liftBase $ atomically $ insert key (Leaf expirationMoment r'') cache
            pure r'

    -- If we got response from cache use it.
    -- If it's old, renew it asynchronously
    case ro of
        Just (SP r' o) -> do
            when o $ liftBaseWith $ \runInIO -> void $ async $ runInIO cachedAction
            liftBase $ wait r'
        Nothing -> cachedAction

data CachePolicy
    = ReturnOld   -- ^ Return elements even outdated, populate cache with new element in background
    | RequestNew  -- ^ If element is outdated, request new and return it
    deriving (Eq, Show, Typeable)

-- | Cached action. Doesn't suffer from cold cache ....
--
-- Simple example:
--
-- @
-- c <- mkCache
-- cachedIO c 10 'k' (putStrLn "foo" >> pure 'v')
-- @
cachedIO
    :: forall c k v. (Cache c, Typeable k, Eq k, Hashable k, Typeable v)
    => c                -- ^ Cache
    -> NominalDiffTime  -- ^ Time-to-live
    -> k                -- ^ Cache key
    -> IO v             -- ^ Value action
    -> IO v
cachedIO = genCachedIO ReturnOld

-- | More general version of 'cachedIO'
genCachedIO
    :: forall c k v. (Cache c, Typeable k, Eq k, Hashable k, Typeable v)
    => CachePolicy
    -> c                -- ^ Cache
    -> NominalDiffTime  -- ^ Time-to-live
    -> k                -- ^ Cache key
    -> IO v             -- ^ Value action
    -> IO v
genCachedIO policy cache ttl key action = do
    now <- getCurrentTime
    let expirationMoment = ttl `addUTCTime` now
    -- Errors are cached for 60 seconds
    let errorExpirationMoment = 60 `addUTCTime` now

    -- Lookup
    ro <- atomically $ lookup' key cache $ \r ->
        case r of
            Nothing -> SP Nothing Nothing
            Just (Leaf stamp r')
                -- If data in cache and recent enough, use it
                | stamp > now ->
                     SP Nothing (Just $ SP r' False)
                -- otherwise, re-insert it with new timestamp so concurrent
                -- requests will still reuse old value.  But return
                -- nothing, so this request will be handled by the actual
                -- application.
                | otherwise -> case policy of
                    ReturnOld  -> SP (Just expirationMoment) (Just $ SP r' True)
                    RequestNew -> SP Nothing Nothing

    --  Action, which updates the cache
    let cachedAction :: IO v
        cachedAction = do
            r'' <- async action :: IO (Async v)
            r' <- atomically $ do
                r' <- waitCatchSTM r'' :: STM (Either SomeException v)
                let em = either (const errorExpirationMoment) (const expirationMoment) r'
                insert key (Leaf em r'') cache
                pure r'
            either throwM pure r'

    -- Initial action, we insert it immediately with short time
    -- if it succeeds we reinsert it with longer moment
    let cachedAction' ::IO v
        cachedAction' = do
            r' <- async action
            -- insert immediately
            atomically $ insert key (Leaf errorExpirationMoment r') cache
            -- wait, if succeeds extend the ttl
            atomically $ do
                e <- waitCatchSTM r'
                case e of
                    Left exc  -> throwM exc
                    Right x -> do
                        insert key (Leaf expirationMoment r') cache
                        pure x

    -- If we got response from cache use it.
    -- If it's old, renew it asynchronously
    case ro of
        Just (SP r' o) -> do
            when o $ liftBaseWith $ \runInIO ->
                void $ async $ runInIO cachedAction
            liftBase $ wait r'
        Nothing -> cachedAction'

-- | Existential cache.
data SomeCache where
    SomeCache :: Cache c => c -> SomeCache

instance Cache SomeCache where
    insert k v (SomeCache c) = insert k v c
    lookup k v (SomeCache c) = lookup k v c
    lookup' k (SomeCache c) s = lookup' k c s

-- | Cache based on 'DynMap.DynMap'.
type DynMapCache = DynMap.DynMap I Leaf

-- | Essentially 'Cache' 'DynMapCache'
instance (fk ~ I, fv ~ Leaf) => Cache (DynMap.DynMap fk fv) where
    insert = DynMap.insert . I
    lookup = DynMap.lookup . I
    -- TODO: implement lookup'
