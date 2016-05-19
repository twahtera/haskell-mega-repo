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
    ) where

import Prelude        ()
import Prelude.Compat hiding (lookup)

import Control.Concurrent.Async    (Async, async, wait)
import Control.Concurrent.STM      (STM, atomically)
import Control.Monad               (void, when)
import Control.Monad.Base          (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import Data.Hashable               (Hashable)
import Data.Proxy                  (Proxy (..))
import Data.Time                   (NominalDiffTime, UTCTime, addUTCTime,
                                    getCurrentTime)
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)

import qualified Servant.Cache.Internal.DynMap as DynMap

-- | Strict pair.
data SP a b = SP !a !b
    deriving (Eq, Ord, Show, Read, Functor, Typeable, Generic)

instance (Hashable a, Hashable b) => Hashable (SP a b)

data Leaf v = Leaf !UTCTime !(Async v)
    deriving (Functor, Typeable, Generic)


-- | Our own 'Identity' to avoid orphan 'Hashable' instance.
newtype I a = I a
    deriving (Eq, Ord, Show, Read, Functor, Typeable, Generic)

instance Hashable a => Hashable (I a)

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

-- | Cached action. Doesn't suffer from cold cache ....
--
-- Simple example:
--
-- @
-- c <- mkCache
-- cachedIO c 10 'k' (putStrLn "foo" >> pure 'v')
-- @
cachedIO :: forall c k v.
            ( Cache c
            , Typeable k, Eq k, Hashable k
            , Typeable v
            )
         => c                -- ^ Cache
         -> NominalDiffTime  -- ^ Time-to-live
         -> k                -- ^ Cache key
         -> IO v              -- ^ Value action
         -> IO v
cachedIO cache ttl key action = do
    now <- getCurrentTime
    let expirationMoment = ttl `addUTCTime` now
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
                | otherwise -> do
                     SP (Just expirationMoment) (Just $ SP r' True)

    --  Action, which updates the cache
    let cachedAction :: IO v
        cachedAction = do
            r'' <- async action :: IO (Async v)
            r' <- wait r'' :: IO v
            atomically $ insert key (Leaf expirationMoment r'') cache
            pure r'

    let cachedAction' ::IO v
        cachedAction' = do
            r' <- async action
            atomically $ insert key (Leaf expirationMoment r') cache
            wait r'

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
