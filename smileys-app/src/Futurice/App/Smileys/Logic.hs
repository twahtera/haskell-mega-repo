{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.Smileys.Logic (getSmileys, postSmiley) where

import Prelude ()
import Futurice.Prelude
import Data.Pool                    (withResource)
import Data.Time                    (getCurrentTime)

import Futurice.Report
import Futurice.App.Smileys.Types
import Futurice.App.Smileys.Ctx

import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Data.Text as T
import qualified FUM
import Servant (ServantErr(..), err403)

getSmileys
    :: (MonadIO m, MonadBaseControl IO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> m SmileysReport
getSmileys ctx _ start end = do
  now <- liftIO $ getCurrentTime
  let today = utctDay now
  withResource (ctxPostgresPool ctx) $ \conn -> do
    res <- liftIO $ Postgres.query conn
        "SELECT entries, username, smiley, day FROM smileys.trail WHERE day >= ? AND day <= ?"
        [fromMaybe today start, fromMaybe today end]
    return $ Report (ReportGenerated now) $ V.fromList $ map toNP res
  where
    toNP (a, b, c, d) = I a :* I b :* I c :* I d :* Nil


postSmiley
    :: (MonadIO m, MonadBaseControl IO m, MonadError ServantErr m)
    => Ctx
    -> Maybe FUM.UserName
    -> PostSmiley
    -> m Res
postSmiley ctx mfum req = do
  withResource (ctxPostgresPool ctx) $ \conn -> do
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        let insertQuery = fromString $ unwords $
               [ "INSERT INTO smileys.trail as c (entries, username, smiley, day)"
               , "VALUES (?, ?, ?, ?) ON CONFLICT (username, day) DO UPDATE"
               , "SET entries = EXCLUDED.entries, smiley = EXCLUDED.smiley"
               ]
        let smiley_entries = textShow $ _postSmileyEntries req
        let smiley_day = textShow $ _postSmileyDate req
        let smiley_user = FUM._getUserName fumUsername
        let smiley_state = textShow $ _postSmileySmiley req
        _ <- liftIO $ Postgres.execute conn
               insertQuery
               [ smiley_entries, smiley_user, smiley_state, smiley_day]
        pure $ Res { _resStatus = "OK" }
