{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillProxy (defaultMain) where

import Data.Pool            (createPool)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import PlanMill.Types.Query (SomeQuery (..))
import Prelude ()
import Servant

import qualified Database.PostgreSQL.Simple as Postgres

-- PlanmillProxy modules
import Futurice.App.PlanMillProxy.API
import Futurice.App.PlanMillProxy.Config (Config (..))
import Futurice.App.PlanMillProxy.Logic
       (cleanupCache, haxlEndpoint, statsEndpoint, updateAllTimereports,
       updateCache, updateCapacities, updateWithoutTimereports)
import Futurice.App.PlanMillProxy.Types  (Ctx (..))

import qualified PlanMill.Queries as PMQ


server :: Ctx -> Server PlanMillProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> liftIO . haxlEndpoint ctx
    :<|> liftIO (statsEndpoint ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName          .~ "Planmill Proxy"
    & serverDescription   .~ "Make faster queries to PlanMill"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverMiddleware    .~ liftFuturiceMiddleware logStdoutDev
    & serverApp planmillProxyApi .~ server
    & serverEnvPfx        .~ "PLANMILLPROXY"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
    makeCtx (Config cfg connectionInfo) logger cache = do
        postgresPool <- createPool
            (Postgres.connect connectionInfo)
            Postgres.close
            2 (10 :: NominalDiffTime) 20 -- stripes, ttl, resources
        let ctx = Ctx
                { ctxCache        = cache
                , ctxPlanmillCfg  = cfg
                , ctxPostgresPool = postgresPool
                , ctxLogger       = logger
                }
        let jobs =
                -- See every 5 minutes, if there's something to update in cache
                [ mkJob "cache update" (updateCache ctx)
                  $ shifted (3 * 60) $ every $ 5 * 60

                -- Cleanup cache every three hours
                , mkJob "cache cleanup" (cleanupCache ctx)
                  $ every $ 180 * 60

                -- Update capacities once in a while
                , mkJob "capacities update" (updateCapacities ctx)
                  $ shifted (15 * 60) $ every $ 15 * 60

                {-
                -- Update recent timereports often
                , ( Job "timereports update" $ updateRecentTimereports ctx
                  , shifted (0 * 60) $ every $ 15 * 60
                  )
                -}
                -- Update timereports
                , mkJob "update timereports" (updateAllTimereports ctx)
                  $ shifted (5 * 60) $ every $ 45 * 60 -- TODO: see how often it should be run

                , mkJob "update without timereports" (updateWithoutTimereports ctx)
                  $ shifted (10 * 60) $ every $ 2 * 60 * 60

                , mkJob "have to be warm" (updateWarmRequests ctx)
                  $ shifted (2 * 60) $ every $ 30 * 60
                ]

        pure (ctx, jobs)

-- | We'll keep some queries warm.
updateWarmRequests :: Ctx -> IO ()
updateWarmRequests ctx = void $ haxlEndpoint ctx
    [ SomeQuery PMQ.usersQuery
    , SomeQuery PMQ.absencesQuery
    ]
