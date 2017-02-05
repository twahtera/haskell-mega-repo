{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Data.Pool        (createPool)
import Futurice.Servant
import Servant

import qualified Database.PostgreSQL.Simple as Postgres

-- ProxyMgmt modules
import Futurice.App.ProxyMgmt.API
import Futurice.App.ProxyMgmt.Config (Config (..))
import Futurice.App.ProxyMgmt.Logic  (accessReport, usersReport)
import Futurice.App.ProxyMgmt.Types

server :: Ctx -> Server ProxyMgmtAPI
server ctx = liftIO (accessReport ctx)
    :<|> liftIO (usersReport ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "Proxy-app management"
    & serverDescription      .~ "Audit log"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC3))
    & serverApp proxyMgmtApi .~ server
    & serverEnvPfx           .~ "PROXYMGMT"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx Config {..} _logger _cache = do
        postgresPool <- createPool
            (Postgres.connect cfgPostgresConnInfo)
            Postgres.close
            1 10 5
        let ctx = Ctx postgresPool
        pure (ctx, [])
