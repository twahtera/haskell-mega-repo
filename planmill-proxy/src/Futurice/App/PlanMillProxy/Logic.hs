{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Futurice.App.PlanMillProxy.Logic (
    haxlEndpoint,
    updateCache,
    cleanupCache,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Catch              (handle)
import Control.Monad.Logger             (LoggingT, filterLogger)
import Control.Monad.PlanMill           (planmillQuery)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, taggedDecode, taggedEncode)
import Data.Constraint
import Data.Pool                        (withResource)
import Futurice.App.PlanMillProxy.H
import Futurice.App.PlanMillProxy.Types (Ctx (..))
import PlanMill.Types                   (Cfg)
import PlanMill.Types.Query             (Query, SomeQuery (..), SomeResponse (..), queryDict)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import qualified Database.PostgreSQL.Simple as Postgres

-------------------------------------------------------------------------------
-- Type synonyms
-------------------------------------------------------------------------------

-- | /TODO/ Store 'SomeResponse' in the database?
type CacheLookup = HashMap SomeQuery BSL.ByteString

lookupCache :: [(SomeQuery, Postgres.Binary BSL.ByteString)] -> CacheLookup
lookupCache ps = HM.fromList (Postgres.fromBinary <$$> ps)

type LIO = LoggingT IO

runLIO :: Ctx -> (Postgres.Connection -> LIO a) -> IO a
runLIO ctx f = withResource (ctxPostgresPool ctx) $ \conn -> runLoggingT' ctx $ f conn

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | The haxl endpoint. We take list of 'Query', and return list of results
haxlEndpoint :: Ctx -> [SomeQuery] -> IO [Either Text SomeResponse]
haxlEndpoint ctx qs = runLIO ctx $ \conn -> do
    _ <- handleSqlError 0 $ Postgres.execute conn viewQuery postgresQs
    cacheResult <- liftIO $ lookupCache <$> Postgres.query conn selectQuery postgresQs
    $(logInfo) $ "Found "
        <> textShow (HM.size cacheResult) <> "/"
        <> textShow (length qs) <> " in postgres"
    traverse (fetch cacheResult conn) qs
  where
    postgresQs = Postgres.Only . Postgres.In $ qs

    -- Fetch provides context for fetch', i.e. this is boilerplate :(
    fetch
        :: CacheLookup -> Postgres.Connection -> SomeQuery
        -> LIO (Either Text SomeResponse)
    fetch cacheResult conn (SomeQuery q) =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' cacheResult conn q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) q

    fetch'
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => CacheLookup  -> Postgres.Connection -> Query a
        -> LIO (Either Text SomeResponse)
    fetch' cacheResult conn q = case HM.lookup (SomeQuery q) cacheResult of
        Just bs -> do
            x <- liftIO $ tryDeep (return . MkSomeResponse q . id' q . taggedDecode $ bs)
            case x of
                Right y -> return (Right y)
                Left exc -> do
                    _ <- handleSqlError 0 $
                        Postgres.execute conn deleteQuery (Postgres.Only q)
                    return $ Left $ show exc ^. packed
        Nothing -> MkSomeResponse q <$$> fetch'' conn q

    -- We use proxy to force the type
    id' :: proxy a -> a -> a
    id' _ = id

    -- Fetch and store
    fetch''
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => Postgres.Connection -> Query a -> LIO (Either Text a)
    fetch'' conn q = do
        res <- liftIO $ tryDeep $ runLoggingT' ctx $ do
            x <- fetchFromPlanMill planmillCfg q
            storeInPostgres conn q x
            pure x
        return $ first (\x -> show x ^. packed) res

    -- Planmill config
    planmillCfg   = ctxPlanmillCfg ctx

    -- Used to update viewed counters
    viewQuery :: Postgres.Query
    viewQuery = "UPDATE planmillproxy.cache SET viewed = viewed + 1 WHERE query in ?;"

    -- Used to delete invalid items (cannot decode)
    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM planmillproxy.cache WHERE query = ?;"

    -- Select multiple items
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT query, data FROM planmillproxy.cache"
        , "WHERE query in ?"
        , ";"
        ]

-- | Update cache, we look what's viewed the most and update these entries.
-- This means that we never delete items from cache
updateCache :: Ctx -> IO ()
updateCache ctx = runLIO ctx $ \conn -> do
    qs <- handleSqlError [] $ Postgres.query_ conn selectQuery
    $(logInfo) $ "Updating " <> textShow (length qs) <> " cache items"
    for_ qs $ \(Postgres.Only (SomeQuery q)) -> fetch conn q
  where
    fetch :: Postgres.Connection -> Query a -> LIO (Either SomeException ())
    fetch conn q =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' conn q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) q

    fetch' conn q = liftIO $ tryDeep $ runLoggingT' ctx $ do
        x <- fetchFromPlanMill planmillCfg q
        storeInPostgres conn q x

    -- Planmill config
    planmillCfg   = ctxPlanmillCfg ctx

    -- Fetch queries which are old enough, and viewed at least once
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT (query) FROM planmillproxy.cache"
        , "WHERE current_timestamp - updated > '10 minutes' AND viewed > 0"
        , "ORDER BY viewed"
        , "LIMIT 1000"
        , ";"
        ]

-- | Cleanup cache
cleanupCache :: Ctx -> IO ()
cleanupCache ctx = runLIO ctx $ \conn -> do
    i <- handleSqlError 0 $ Postgres.execute_ conn cleanupQuery
    $(logInfo) $  "cleaned up " <> textShow i <> " cache items"
  where
    cleanupQuery :: Postgres.Query
    cleanupQuery = fromString $ unwords $
        [ "DELETE FROM planmillproxy.cache"
        , "WHERE current_timestamp - updated > '6 hours' AND viewed <= 0"
        , ";"
        ]

storeInPostgres
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a)
    => Postgres.Connection -> Query a -> a -> LIO ()
storeInPostgres conn q x = do
    i <- handleSqlError 0 $
        Postgres.execute conn postgresQuery (q, Postgres.Binary $ taggedEncode x)
    when (i == 0) $
        $(logWarn) $ "Storing in postgres failed: " <> show q ^. packed
  where
    postgresQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data, viewed = 0, updated = now()"
        , "WHERE c.query = EXCLUDED.query"
        , ";"
        ]

-- | Run query on real planmill backend.
fetchFromPlanMill :: Cfg -> Query a -> LIO a
fetchFromPlanMill cfg q = liftIO $ runH cfg $ planmillQuery q

handleSqlError :: a -> IO a -> LIO a
handleSqlError x action = handle (omitSqlError x) $ liftIO action

omitSqlError :: a -> Postgres.SqlError -> LIO a
omitSqlError a err = do
    $(logError) $ textShow err
    return a

runLoggingT' :: Ctx -> LoggingT IO a -> IO a
runLoggingT' ctx l =
    runStderrLoggingT $ filterLogger p l
  where
    p _ level = level >= ctxLogLevel ctx
