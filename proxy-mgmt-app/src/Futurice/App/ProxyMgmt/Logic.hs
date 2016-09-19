{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt.Logic (
    accessReport,
    usersReport,
    ) where

import Data.Pool                    (withResource)
import Data.Time                    (getCurrentTime)
import Futurice.App.ProxyMgmt.Types
import Futurice.Prelude
import Futurice.Report
import Generics.SOP                 (I (..), NP (..))
import Prelude ()

import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as Postgres

accessReport :: Ctx -> IO AccessReport
accessReport Ctx { ctxPostgresPool = pool } = do
    now <- getCurrentTime
    withResource pool $ \conn -> do
        res <- Postgres.query_ conn
            "SELECT username, updated, endpoint FROM proxyapp.accesslog ORDER BY updated DESC LIMIT 1000;"
        return $ Report (ReportGenerated now) $ V.fromList $ map toNP res
  where
    toNP (a, b, c) = I a :* I b :* I c :* Nil

usersReport :: Ctx -> IO UsersReport
usersReport Ctx { ctxPostgresPool = pool } = do
    now <- getCurrentTime
    withResource pool $ \conn -> do
        res <- Postgres.query_ conn
            "SELECT username, createdby, createdat FROM proxyapp.credentials;"
        return $ Report (ReportGenerated now) $ V.fromList $ map toNP res
  where
    toNP (a, b, c) = I a :* I b :* I c :* Nil
