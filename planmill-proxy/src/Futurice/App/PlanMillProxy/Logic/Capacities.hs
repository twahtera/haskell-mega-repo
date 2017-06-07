{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillProxy.Logic.Capacities (
    selectCapacities,
    updateCapacities,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Binary.Tagged        (taggedDecode, taggedEncode)
import Data.Time                 (addDays)
import Futurice.PostgresPool
import Numeric.Interval.NonEmpty (inf, sup, (...))
import PlanMill.Types.Query      (Query (..))

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Numeric.Interval.NonEmpty  as Interval
import qualified PlanMill                   as PM

import Futurice.App.PlanMillProxy.Logic.Common
import Futurice.App.PlanMillProxy.Types        (Ctx (..))

-- | /TODO/ make fallback do async. Return zero capacities instead
selectCapacities
    :: Ctx
    -> PM.UserId -> PM.Interval Day -> LIO PM.UserCapacities
selectCapacities ctx uid interval = do
    res <- handleSqlError [] $ poolQuery ctx selectQuery (uid, inf interval, sup interval)
    if (length res /= intervalLength)
        then do
            logInfo_ $
                "Less entries for " <> textShow interval <>
                " capacity interval for " <> textShow uid <>
                ": " <> textShow (length res) <> "/" <> textShow intervalLength
            fallback
        else do
            res' <- liftIO $ tryDeep $ return $! V.fromList $ map selectTransform res
            case res' of
                Right x  -> return x
                Left exc -> do
                    logAttention_ $ "selectCapacities: " <> textShow exc
                    fallback
  where
    fallback = do
        -- We get an extended interval
        x <- fetchFromPlanMill ctx q
        i <- handleSqlError 0 $ poolExecuteMany ctx insertQuery (transformForInsert x)
        when (fromIntegral i /= length x) $
            logAttention_ $ "Inserted less capacities than we got from planmill"
        -- ... so we trim the result
        let x' = V.filter (\c -> PM.userCapacityDate c `Interval.member` interval) x
        return $! x'

    -- Interval is inclusive on both ends
    intervalLength = fromEnum (sup interval) - fromEnum (inf interval) + 1

    transformForInsert
        :: PM.UserCapacities
        -> [(PM.UserId, Day, Postgres.Binary BSL.ByteString)]
    transformForInsert = fmap f . toList
      where
        f uc = (uid, PM.userCapacityDate uc, Postgres.Binary $ taggedEncode uc)

    insertQuery :: Postgres.Query
    insertQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.capacity as c (uid, day, data)"
        , "VALUES (?, ?, ?)"
        , "ON CONFLICT (uid, day) DO UPDATE"
        , "SET  data = EXCLUDED.data, updated = current_timestamp"
        , "WHERE c.uid = EXCLUDED.uid AND c.day = EXCLUDED.day"
        ]

    -- If we gonna planmill we ask some time into a future and the past
    q :: Query PM.UserCapacities
    q = QueryCapacities interval' uid
      where
        interval' = addDays (-7) (inf interval) ... addDays 7 (sup interval)

    selectTransform
        :: Postgres.Only (Postgres.Binary BSL.ByteString)
        -> PM.UserCapacity
    selectTransform (Postgres.Only (Postgres.Binary bs)) = taggedDecode bs

    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.capacity"
        , "WHERE uid = ? AND day >= ? AND day <= ?"
        , ";"
        ]

updateCapacities :: Ctx -> IO ()
updateCapacities ctx = runLIO ctx $ do
    old <- liftIO $ poolQuery_ ctx selectQuery
    for_ old $ \(uid, mi, ma) ->
        void $ selectCapacities ctx uid (mi ... ma)
  where
    -- select intervals which are 6...12 hours old data
    -- clamp the max date to be in current-day + (1 month ... 6 month) interval
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT uid, MIN(day) as min, LEAST(GREATEST(current_date + '1 month' :: interval, MAX(day)), current_date + '6 months' :: interval) :: date as max"
        , "FROM planmillproxy.capacity"
        , "WHERE current_timestamp - updated > (" ++ capacityAge ++ " :: interval) * (1 + variance)"
        , "GROUP BY uid"
        , ";"
        ]
