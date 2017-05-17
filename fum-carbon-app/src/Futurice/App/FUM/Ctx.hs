{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Ctx (
    Ctx (..),
    newCtx,
    ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Pool              (Pool, createPool)
import Futurice.CryptoRandom  (CryptoGen, mkCryptoGen)
import Futurice.Prelude
import Prelude ()

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Personio

import Futurice.App.FUM.Types


data Ctx = Ctx
    { ctxLogger      :: !Logger
    , ctxPersonio    :: !([Personio.Employee])
    , ctxWorld       :: !(TVar World)
    , ctxPostgres    :: !(Pool Postgres.Connection)
    , ctxPRNGs       :: !(Pool (TVar CryptoGen))
    -- , ctxMockUser    :: !(Maybe FUM.UserName)
    }

newCtx
    :: Logger
    -> [Personio.Employee]
    -> Postgres.ConnectInfo
    -> World
    -> IO Ctx
newCtx logger es ci w = Ctx logger es
    <$> newTVarIO w
    <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
    <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
