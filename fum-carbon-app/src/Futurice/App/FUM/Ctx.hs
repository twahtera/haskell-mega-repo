{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.FUM.Ctx (
    Ctx (..),
    newCtx,
    ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Pool              (Pool, createPool)
import Futurice.CryptoRandom  (CryptoGen, mkCryptoGen)
import Futurice.IdMap         (IdMap)
import Futurice.Prelude
import Prelude ()

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Personio

import Futurice.App.FUM.Types


data Ctx = Ctx
    { ctxLogger              :: !Logger
    , ctxPersonio            :: !(TVar (IdMap Personio.Employee))
    , ctxPersonioValidations :: !(IO [Personio.EmployeeValidation])
    , ctxWorld               :: !(TVar World)
    , ctxPostgres            :: !(Pool Postgres.Connection)
    , ctxPRNGs               :: !(Pool (TVar CryptoGen))
    -- , ctxMockUser    :: !(Maybe FUM.UserName)
    }

newCtx
    :: Logger
    -> Postgres.ConnectInfo
    -> IdMap Personio.Employee
    -> IO [Personio.EmployeeValidation]
    -> World
    -> IO Ctx
newCtx logger ci es vs w = Ctx logger
    <$> newTVarIO es
    <*> pure vs
    <*> newTVarIO w
    <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
    <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
