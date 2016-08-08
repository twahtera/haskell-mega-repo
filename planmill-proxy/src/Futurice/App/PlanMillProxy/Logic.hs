{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.PlanMillProxy.Logic (
    haxlEndpoint,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Catch              (handle)
import Control.Monad.PlanMill           (planmillQuery)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, taggedDecode, taggedEncode)
import Data.ByteString.Lazy             (ByteString)
import Data.Constraint
import Data.Pool                        (withResource)
import Futurice.App.PlanMillProxy.H
import Futurice.App.PlanMillProxy.Types (Ctx (..))
import PlanMill.Types                   (Cfg)
import PlanMill.Types.Query             (Query, SomeQuery (..), queryDict)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import qualified Database.PostgreSQL.Simple as Postgres

type CacheLookup = HashMap SomeQuery BSL.ByteString

lookupCache :: [(SomeQuery, Postgres.Binary BSL.ByteString)] -> CacheLookup
lookupCache ps = HM.fromList (Postgres.fromBinary <$$> ps)

haxlEndpoint :: Ctx -> [SomeQuery] -> IO [Either Text ByteString]
haxlEndpoint ctx qs = withResource (ctxPostgresPool ctx) $ \conn -> do
    _ <- Postgres.execute conn viewQuery postgresQs
    cacheResult <- lookupCache <$> Postgres.query conn selectQuery postgresQs
    print $ HM.size cacheResult -- $ log how many we found
    traverse (fetch cacheResult conn) qs
  where
    postgresQs = Postgres.Only . Postgres.In $ qs

    -- Fetch provides context for fetch', i.e. this is boilerplate :(
    fetch
        :: CacheLookup -> Postgres.Connection -> SomeQuery
        -> IO (Either Text ByteString)
    fetch cacheResult conn (SomeQuery q) =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' cacheResult conn q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) (Sub Dict) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) (Sub Dict) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) (Sub Dict) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) (Sub Dict) q

    fetch'
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => CacheLookup  -> Postgres.Connection -> Query a
        -> IO (Either Text ByteString)
    fetch' cacheResult conn q = case HM.lookup (SomeQuery q) cacheResult of
        Just bs -> do
            x <- tryDeep (return . taggedEncode . id' q . taggedDecode $ bs)
            case x of
                Right y -> return (Right y)
                Left exc -> do
                    _ <- handle (omitSqlError 0) $
                        Postgres.execute conn deleteQuery (Postgres.Only q)
                    return $ Left $ show exc ^. packed
        Nothing -> taggedEncode <$$> fetch'' conn q

    -- We use proxy to force the type
    id' :: proxy a -> a -> a
    id' _ = id

    -- Fetch and store
    fetch''
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => Postgres.Connection -> Query a -> IO (Either Text a)
    fetch'' conn q = do
        res <- tryDeep $ do
            x <- fetchFromPlanMill planmillCfg q
            storeInPostgres conn q x
            pure x
        return $ first (\x -> show x ^. packed) res

    -- Planmill config
    planmillCfg   = ctxPlanmillCfg ctx

    -- Used to update viewed counters
    viewQuery :: Postgres.Query
    viewQuery = "UPDATE planmillproxy.cache SET viewed = viewed + 1 WHERE query in ?"

    -- Used to delete invalid items (cannot decode)
    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM planmillproxy.cache WHERE query = ?;"

    -- Select multiple items
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT query, data FROM planmillproxy.cache"
        , "WHERE query in ?;"
        ]

storeInPostgres
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a)
    => Postgres.Connection -> Query a -> a -> IO ()
storeInPostgres conn q x = do
    i <- handle (omitSqlError 0) $
        Postgres.execute conn postgresQuery (q, Postgres.Binary b)
    print i -- log!
  where
    postgresQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data, viewed = 0, updated = now()"
        , "WHERE c.query = EXCLUDED.query;"
        ]

    b =  taggedEncode x

-- | Run query on real planmill backend.
fetchFromPlanMill :: Cfg -> Query a -> IO a
fetchFromPlanMill cfg q = runH cfg $ planmillQuery q

omitSqlError :: a -> Postgres.SqlError -> IO a
omitSqlError a err = do
    print err
    -- $(logError) $ textShow err
    return a
