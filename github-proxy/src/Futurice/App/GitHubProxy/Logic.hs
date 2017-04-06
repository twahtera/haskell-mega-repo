{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
module Futurice.App.GitHubProxy.Logic (
    -- * Endpoint
    haxlEndpoint,
    -- * Generic cache
    updateCache,
    cleanupCache,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Catch            (handle)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, taggedDecode, taggedEncode)
import Data.Constraint
import Futurice.App.GitHubProxy.H     (runH)
import Futurice.App.GitHubProxy.Types (Ctx (..))
import Futurice.GitHub
       (Auth, RW (..), ReqTag, Request, SomeRequest (..), SomeResponse (..),
       tagDict)
import Futurice.Integrations.Classes  (MonadGitHub (..))
import Futurice.PostgresPool
import Futurice.Servant
       (CachePolicy (..), DynMapCache, genCachedIO)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import qualified Database.PostgreSQL.Simple as Postgres

import Data.Binary.Get    (Get, runGetOrFail)
import Data.Binary.Tagged
       (SemanticVersion, Version, structuralInfo,
       structuralInfoSha1ByteStringDigest)
import GHC.TypeLits       (natVal)

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

genericAge :: String
genericAge = "'6 hours'"

-------------------------------------------------------------------------------
-- Type synonyms
-------------------------------------------------------------------------------

-- | /TODO/ Store 'SomeResponse' in the database?
type CacheLookup = HashMap SomeRequest BSL.ByteString

lookupCache :: [(SomeRequest, Postgres.Binary BSL.ByteString)] -> CacheLookup
lookupCache ps = HM.fromList (Postgres.fromBinary <$$> ps)

type LIO = LogT IO

runLIO :: Ctx -> LIO a -> IO a
runLIO = runLogT'

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | The haxl endpoint. We take list of 'Query', and return list of results
haxlEndpoint :: Ctx -> [SomeRequest] -> IO [Either Text SomeResponse]
haxlEndpoint ctx qs = runLIO ctx $ do
    -- Optimistically update view counts
    _ <- handleSqlError 0 $ poolExecute ctx viewQuery postgresQs

    -- Hit the cache for non-primitive queries
    cacheResult <- liftIO $ lookupCache <$> poolQuery ctx selectQuery postgresQs

    -- Info about cache success
    logInfo_ $ "Found "
        <> textShow (HM.size cacheResult) <> " / "
        <> textShow (length qs) <> " (found/all) query results in postgres"

    -- go thru each request
    -- primitive requests are handled one by one, that could be optimised.
    res <- traverse (fetch cacheResult) qs

    -- Log the first errorneous response
    case (res ^? folded . _Left) of
        Nothing  -> pure ()
        Just err -> logAttention_ $ "haxl response contains errors : " <> err

    -- return
    pure res
  where
    postgresQs  = Postgres.Only . Postgres.In $ qs

    -- Fetch provides context for fetch', i.e. this is boilerplate :(
    fetch
        :: CacheLookup -> SomeRequest
        -> LIO (Either Text SomeResponse)
    fetch cacheResult (MkSomeRequest tag req) =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' cacheResult tag req
      where
        binaryDict = tagDict (Proxy :: Proxy Binary) tag
        semVerDict = tagDict (Proxy :: Proxy HasSemanticVersion) tag
        structDict = tagDict (Proxy :: Proxy HasStructuralInfo) tag
        nfdataDict = tagDict (Proxy :: Proxy NFData) tag

    fetch'
        :: forall a. (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => CacheLookup  -> ReqTag a -> Request 'RA a
        -> LIO (Either Text SomeResponse)
    fetch' cacheResult tag req = case HM.lookup sreq cacheResult of
        Just bs -> do
            -- we only check tags, the rest of the response is decoded lazily
            -- Hopefully when the end result is constructed.
            if checkTagged (Proxy :: Proxy a) bs
                then pure $ Right $ MkSomeResponse tag $ taggedDecode bs
                else do
                    logAttention_ $ "Borked cache content for " <> textShow sreq
                    _ <- handleSqlError 0 $
                        poolExecute ctx deleteQuery (Postgres.Only sreq)
                    return $ Left $ "structure tags don't match"
        Nothing -> MkSomeResponse tag <$$> fetch'' tag req
      where
        sreq = MkSomeRequest tag req

    -- Fetch and store
    fetch''
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => ReqTag a -> Request 'RA a -> LIO (Either Text a)
    fetch'' tag req = do
        res <- liftIO $ tryDeep $ runLogT' ctx $ do
            x <- fetchFromGitHub (ctxLogger ctx) (ctxCache ctx) (ctxGitHubAuth ctx) tag req
            storeInPostgres ctx tag req x
            pure $! x
        -- liftIO $ print (res ^? _Left, q)
        return $ first (\x -> ("non-primitive query failure " <> show sreq <> " " <> show x) ^. packed) res
      where
        sreq = MkSomeRequest tag req

    -- Used to update viewed counters
    viewQuery :: Postgres.Query
    viewQuery = "UPDATE githubproxy.cache SET viewed = viewed + 1 WHERE query in ?;"

    -- Used to delete invalid items (cannot decode)
    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM githubproxy.cache WHERE query = ?;"

    -- Select multiple items
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT query, data FROM githubproxy.cache"
        , "WHERE query in ?"
        , ";"
        ]

-- | Update cache, we look what's viewed the most and update these entries.
-- This means that we never delete items from cache
updateCache :: Ctx -> IO ()
updateCache ctx = runLIO ctx $ do
    qs <- handleSqlError [] $ poolQuery_ ctx selectQuery
    logInfo_ $ "Updating " <> textShow (length qs) <> " cache items"
    for_ qs $ \(Postgres.Only (MkSomeRequest tag req)) -> fetch tag req
  where
    fetch :: ReqTag a -> Request 'RA a -> LIO (Either SomeException ())
    fetch tag req  =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' tag req
      where
        binaryDict = tagDict (Proxy :: Proxy Binary) tag
        semVerDict = tagDict (Proxy :: Proxy HasSemanticVersion) tag
        structDict = tagDict (Proxy :: Proxy HasStructuralInfo) tag
        nfdataDict = tagDict (Proxy :: Proxy NFData) tag

    fetch'
      :: (Binary a, HasStructuralInfo a, HasSemanticVersion a)
      => ReqTag a -> Request 'RA a -> LIO (Either SomeException ())
    fetch' tag req = liftIO $ tryDeep $ runLogT' ctx $ do
        x <- fetchFromGitHub (ctxLogger ctx) (ctxCache ctx) (ctxGitHubAuth ctx) tag req
        storeInPostgres ctx tag req x

    -- Fetch queries which are old enough, and viewed at least once
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT (query) FROM githubproxy.cache"
        , "WHERE current_timestamp - updated > (" ++ genericAge ++ " :: interval) * (1 + variance) AND viewed > 0"
        , "ORDER BY viewed"
        , "LIMIT 1000"
        , ";"
        ]

-- | Cleanup cache
cleanupCache :: Ctx -> IO ()
cleanupCache ctx = runLIO ctx $ do
    i <- handleSqlError 0 $ poolExecute_ ctx cleanupQuery
    logInfo_ $  "cleaned up " <> textShow i <> " cache items"
  where
    cleanupQuery :: Postgres.Query
    cleanupQuery = fromString $ unwords $
        [ "DELETE FROM githubproxy.cache"
        , "WHERE current_timestamp - updated > '24 hours' AND viewed <= 0"
        , ";"
        ]

storeInPostgres
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a, HasPostgresPool ctx)
    => ctx -> ReqTag a -> Request 'RA a -> a -> LIO ()
storeInPostgres ctx tag req x = do
    -- -- logInfo_ $ "Storing in postgres" <> textShow q
    i <- handleSqlError 0 $
        poolExecute ctx postgresQuery (MkSomeRequest tag req, Postgres.Binary $ taggedEncode x)
    when (i == 0) $
        logAttention_ $ "Storing in postgres failed: " <> textShow (MkSomeRequest tag req)
  where
    postgresQuery = fromString $ unwords $
        [ "INSERT INTO githubproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data, viewed = 0, updated = now(), variance = random()"
        , "WHERE c.query = EXCLUDED.query"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Utiltities
-------------------------------------------------------------------------------

-- | Run query on real planmill backend.
fetchFromGitHub :: Logger -> DynMapCache -> Auth -> ReqTag a -> Request 'RA a -> LIO a
fetchFromGitHub logger cache auth tag req = case (typeableDict, eqDict) of
    (Dict, Dict) -> liftIO
        -- TODO: add cache cleanup
        $ genCachedIO RequestNew logger cache (10 * 60) req
        $ runH auth $ githubReq req
  where
    typeableDict = tagDict (Proxy :: Proxy Typeable) tag
    eqDict = tagDict (Proxy :: Proxy Eq) tag

handleSqlError :: a -> IO a -> LIO a
handleSqlError x action = handle (omitSqlError x) $ liftIO action

omitSqlError :: a -> Postgres.SqlError -> LIO a
omitSqlError a err = do
    logAttention_ $ textShow err
    return a

runLogT' :: Ctx -> LogT IO a -> IO a
runLogT' ctx = runLogT "github-proxy" (ctxLogger ctx)

-------------------------------------------------------------------------------
-- binary-tagged additions
-------------------------------------------------------------------------------

-- | Check whether the tag at the beginning of the 'LazyByteString' is correct.
checkTagged
    :: forall a. (HasStructuralInfo a, HasSemanticVersion a)
    => Proxy a -> LazyByteString -> Bool
checkTagged _ lbs = either (const False) (view _3) $ runGetOrFail decoder lbs
  where
    decoder :: Get Bool
    decoder = do
        ver <- get
        hash' <- get
        pure $ ver == ver' && hash' == hash''

    proxyV = Proxy :: Proxy (SemanticVersion a)
    proxyA = Proxy :: Proxy a
    ver' = fromIntegral (natVal proxyV) :: Version
    hash'' = structuralInfoSha1ByteStringDigest . structuralInfo $ proxyA
