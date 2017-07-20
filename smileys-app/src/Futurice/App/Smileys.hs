{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Smileys (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Data.Pool        (createPool)
import Futurice.Servant
import Servant

import qualified Database.PostgreSQL.Simple as Postgres

-- Smileys modules
import Futurice.App.Smileys.API
import Futurice.App.Smileys.Charts
import Futurice.App.Smileys.Config (Config (..))
import Futurice.App.Smileys.Ctx
import Futurice.App.Smileys.Logic

server :: Ctx -> Server SmileysAPI
server ctx = pure "smileys backend"
    :<|> postOwnSmileys ctx
    :<|> getOwnSmileys ctx
    :<|> getSmileys ctx
    :<|> absoluteChartHandler ctx
    :<|> relativeChartHandler ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "Smileys-app"
    & serverDescription      .~ "Hours Smileys"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC3))
    & serverApp smileysApi .~ server
    & serverEnvPfx           .~ "SMILEYS"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx Config {..} logger cache = do
        postgresPool <- createPool
            (Postgres.connect cfgPostgresConnInfo)
            Postgres.close
            1 10 5
        let ctx = Ctx
                  { ctxPostgresPool = postgresPool
                  , ctxMockUser     = cfgMockUser
                  , ctxLogger       = logger
                  , ctxCache        = cache
                  }
        pure (ctx, [])
