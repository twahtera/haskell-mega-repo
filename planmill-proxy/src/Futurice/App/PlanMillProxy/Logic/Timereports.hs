{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.PlanMillProxy.Logic.Timereports (
    selectTimereports,
    updateAllTimereports,
    updateWithoutTimereports,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Binary.Tagged        (taggedDecode, taggedEncode)
import Futurice.PostgresPool
import Numeric.Interval.NonEmpty (inf, sup, (...))
import PlanMill.Queries          (usersQuery)
import PlanMill.Types.Query      (Query (..))

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM

import Futurice.App.PlanMillProxy.Logic.Common
import Futurice.App.PlanMillProxy.Types        (Ctx (..))

-- | Select timereports
--
-- If data in cache is invalid, we prune it, and return zero timereports.
selectTimereports
    :: HasPostgresPool ctx => ctx
    -> PM.UserId -> Maybe (PM.Interval Day) -> LIO PM.Timereports
selectTimereports ctx uid minterval = do
    res <- handleSqlError [] $ case minterval of
        Nothing       -> poolQuery ctx selectQueryWithoutInterval (Postgres.Only uid)
        Just interval -> poolQuery ctx selectQueryWithInterval (uid, inf interval, sup interval)
    res' <- liftIO $ tryDeep $ return $ V.fromList $ map selectTransform res
    case res' of
        Right x -> return x
        Left exc -> do
            logAttention_ $ "selectTimereports: " <> textShow exc
            _ <- handleSqlError 0 $ case minterval of
                Nothing       -> poolExecute ctx deleteQueryWithoutInterval (Postgres.Only uid)
                Just interval -> poolExecute ctx deleteQueryWithInterval (uid, inf interval, sup interval)
            return mempty
  where
    selectTransform
        :: Postgres.Only (Postgres.Binary BSL.ByteString)
        -> PM.Timereport
    selectTransform (Postgres.Only (Postgres.Binary bs)) = taggedDecode bs

    selectQueryWithInterval :: Postgres.Query
    selectQueryWithInterval = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.timereports"
        , "WHERE uid = ? AND day >= ? AND day <= ?"
        , ";"
        ]

    selectQueryWithoutInterval :: Postgres.Query
    selectQueryWithoutInterval = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]

    deleteQueryWithoutInterval :: Postgres.Query
    deleteQueryWithoutInterval = fromString $ unwords $
        [ "DELETE FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]

    deleteQueryWithInterval :: Postgres.Query
    deleteQueryWithInterval = fromString $ unwords $
        [ "DELETE FROM planmillproxy.timereports"
        , "WHERE uid = ? AND day >= ? AND day <= ?"
        , ";"
        ]

-- | Update timereports for people without any timereports
updateWithoutTimereports
    :: Ctx -> IO ()
updateWithoutTimereports ctx = runLIO ctx $ do
    logInfo_ $ "Selecting timereports for users without any"

    allUsers <- fetchFromPlanMill ctx usersQuery
    let allUidsSet = Set.fromList $ allUsers ^.. traverse . PM.identifier

    uids <- Postgres.fromOnly <$$> handleSqlError [] (poolQuery_ ctx selectUsersQuery)
    let uidsSet = Set.fromList uids

    for_ (Set.difference allUidsSet uidsSet) (updateTimereportsForUser ctx)
  where
    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT uid FROM planmillproxy.timereports GROUP BY uid"
        ]

-- | Update timereports.
updateAllTimereports
    :: Ctx -> IO ()
updateAllTimereports ctx = runLIO ctx $ do
    logInfo_ $ "Updating timereports for users"

    -- Select uids with oldest updated time reports
    uids <- Postgres.fromOnly <$$> handleSqlError [] (poolQuery_ ctx selectUsersQuery)
    logInfo_ $ "Updating timereports for users: " <>
        T.intercalate ", " (textShow . getIdent <$> uids)

    for_ uids (updateTimereportsForUser ctx)
  where
    getIdent (PM.Ident a) = a

    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT u.uid FROM "
        , "(SELECT uid, MIN(updated) as updated FROM planmillproxy.timereports GROUP BY uid) AS u"
        , "ORDER BY u.updated ASC LIMIT 30"
        , ";"
        ]

updateTimereportsForUser :: Ctx -> PM.UserId -> LIO ()
updateTimereportsForUser ctx uid = do
    let interval = $(mkDay "2015-01-01") ... $(mkDay "2017-12-31")
    let q = QueryTimereports (Just interval) uid
    --
    -- Fetch timereports from planmill
    tr <- fetchFromPlanMill ctx q

    -- Check what timereports we have stored, remove ones not in planmill anymore
    let planmillTrids = Set.fromList (tr ^.. traverse . PM.identifier)
    postgresTrids <- toTrids <$> handleSqlError []
        (poolQuery ctx selectQuery $ Postgres.Only uid)

    let notInPlanmill = Set.difference postgresTrids planmillTrids
    when (not $ Set.null notInPlanmill) $ do
        let notInPlanmillCount = Set.size notInPlanmill
        logInfo_ $
            "Found " <> textShow notInPlanmillCount <>
            " timereports not in planmill anymore"
        i <- handleSqlError 0 $ poolExecute ctx deleteQuery
            (Postgres.Only $ Postgres.In $ Set.toList notInPlanmill)
        when (fromIntegral i /= notInPlanmillCount) $
            logAttention_ $
                "Deleted " <> textShow i <>
                " out of " <> textShow notInPlanmillCount <> " timereports"

    -- Insert timereports
    _ <- handleSqlError 0 $ insertTimereports ctx tr

    -- Done
    pure ()
  where
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT trid FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]

    toTrids :: [Postgres.Only PM.TimereportId] -> Set PM.TimereportId
    toTrids = Set.fromList . map Postgres.fromOnly

    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM planmillproxy.timereports WHERE trid IN ?;"

{-
updateRecentTimereports :: Ctx -> IO ()
updateRecentTimereports ctx = runLIO ctx $ \conn -> do
    users <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) usersQuery
    let uids = users ^.. traverse . PM.identifier

    -- For each user...
    for_ uids $ \uid -> do
        [(mi, ma)] <- liftIO $ poolQuery ctx selectQuery (Postgres.Only uid)
        logInfo_ $ "Last updated timereports at " <> textShow (mi :: UTCTime)

        let q = timereportsModifiedQuery uid mi ma
        timereports <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) q
        _ <- handleSqlError 0 $ insertTimereports conn timereports

        pure ()
  where
    selectQuery = fromString $ unwords
        [ "SELECT coalesce(max(updated), current_timestamp - ('2 months' :: interval)) - ('1 hour' :: interval), current_timestamp"
        , "FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]
-}

-- Helper function to insert timereports
insertTimereports
    :: (Foldable f, HasPostgresPool ctx)
    => ctx
    -> f PM.Timereport
    -> IO Int64
insertTimereports ctx trs =
    poolExecuteMany ctx insertQuery $ transformForInsert <$> toList trs
  where
    transformForInsert
        :: PM.Timereport
        -> (PM.TimereportId, PM.UserId, Day, Postgres.Binary BSL.ByteString)
    transformForInsert tr =
        ( tr ^. PM.identifier
        , PM.trPerson tr
        , PM.trStart tr
        , Postgres.Binary $ taggedEncode tr
        )

    insertQuery :: Postgres.Query
    insertQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.timereports as tr (trid, uid, day, data)"
        , "VALUES (?, ?, ?, ?)"
        , "ON CONFLICT (trid) DO UPDATE"
        , "SET uid = EXCLUDED.uid, day = EXCLUDED.day, data = EXCLUDED.data, updated = now(), variance = random()"
        , "WHERE tr.trid = EXCLUDED.trid"
        , ";"
        ]
