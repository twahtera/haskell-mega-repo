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
       (HasSemanticVersion, HasStructuralInfo, taggedEncode)
import Data.ByteString.Lazy             (ByteString)
import Data.Constraint
import Data.Pool                        (withResource)
import Futurice.App.PlanMillProxy.H
import Futurice.App.PlanMillProxy.Types (Ctx (..))
import Futurice.Servant                 (cachedIO)
import PlanMill.Types                   (Cfg)
import PlanMill.Types.Query             (Query, SomeQuery (..), queryDict)

import qualified Data.Aeson                 as Aeson
import qualified Database.PostgreSQL.Simple as Postgres

haxlEndpoint :: Ctx -> [SomeQuery] -> IO [Either Text ByteString]
haxlEndpoint ctx qs = withResource (ctxPostgresPool ctx) $ \conn ->
    traverse (fetch conn) qs
  where
    fetch :: Postgres.Connection -> SomeQuery -> IO (Either Text ByteString)
    fetch conn (SomeQuery q) =
        case (binaryDict, semVerDict, structDict, nfdataDict, typeableDict) of
            (Dict, Dict, Dict, Dict, Dict) -> taggedEncode <$$> fetch' conn q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) (Sub Dict) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) (Sub Dict) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) (Sub Dict) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) (Sub Dict) q
        typeableDict = queryDict (Proxy :: Proxy Typeable) (Sub Dict) q

    fetch'
        :: (NFData a, Typeable a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => Postgres.Connection -> Query a -> IO (Either Text a)
    fetch' conn q = do
        res <- tryDeep $ fetchCached ctx conn q
        return $ first (\x -> show x ^. packed) res

-- | Run query trhu cache.
fetchCached
    :: forall a. (Typeable a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
    => Ctx -> Postgres.Connection -> Query a -> IO a
fetchCached ctx conn q = cachedIO cache 3600 q $ do
    x <- fetchFromPlanMill cfg q
    storeInPostgres x
    pure x
  where
    cache = ctxCache ctx
    cfg   = ctxPlanmillCfg ctx
    aesonQuery = Aeson.encode q

    storeInPostgres :: a -> IO ()
    storeInPostgres x = do
        i <- handle (omitSqlError 0) $
            Postgres.execute conn postgresQuery (aesonQuery, b)
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
