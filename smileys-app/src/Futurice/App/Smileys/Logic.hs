{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.Smileys.Logic (getSmileys, postSmiley) where

import Prelude ()
import Futurice.Prelude
import Data.Pool        (withResource)

import Futurice.App.Smileys.Types
import Futurice.App.Smileys.Ctx

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM
import Servant (ServantErr(..), err403)

getSmileys
    :: (MonadIO m, MonadBaseControl IO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> m [Smileys]
getSmileys ctx _ start end = do
    now <- liftIO $ currentTime
    let today = utctDay now
    withResource (ctxPostgresPool ctx) $ \conn -> do
        res <- q conn (fromMaybe today start) (fromMaybe today end)
        return $ res
        where
          q :: (MonadIO m, MonadBaseControl IO m) => Postgres.Connection -> Day -> Day -> m [Smileys]
          q conn s e = liftIO $ Postgres.query conn
                "SELECT entries, username, smiley, day FROM smileys.trail WHERE day >= ? AND day <= ?"
                (s, e)

postSmiley
    :: (MonadIO m, MonadBaseControl IO m, MonadError ServantErr m)
    => Ctx
    -> Maybe FUM.UserName
    -> PostSmiley
    -> m Res
postSmiley ctx mfum req = do
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        withResource (ctxPostgresPool ctx) $ \conn -> do
            let insertQuery = fromString $ unwords $
                 [ "INSERT INTO smileys.trail as c (entries, username, smiley, day)"
                 , "VALUES (?, ?, ?, ?) ON CONFLICT (username, day) DO UPDATE"
                 , "SET entries = EXCLUDED.entries, smiley = EXCLUDED.smiley"
                 ]
            let smiley_entries = _postSmileyEntries req
            let smiley_day = textShow $ _postSmileyDate req
            let smiley_user = FUM._getUserName fumUsername
            let smiley_state = textShow $ _postSmileySmiley req
            _ <- liftIO $ Postgres.execute conn
                     insertQuery
                     ( smiley_entries, smiley_user, smiley_state, smiley_day)
            pure $ Res { _resStatus = "OK" }
