{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.FUM (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM    (readTVarIO)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Config
import Futurice.App.FUM.Types
import Futurice.App.FUM.Types.Ctx
import Futurice.App.FUM.Pages.Index

import qualified Personio

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Ctx -> Server ChecklistAPI
server ctx = indexPageImpl ctx

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> a
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx fu  = withAuthUser ctx fu impl
  where
    impl world es = pure $ indexPage world es

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => Ctx
    -> auth
    -> (World -> [Personio.Employee] -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser ctx fu f = runLogT "withAuthUser" (ctxLogger ctx) $
    withAuthUser' (error "forbiddenPage") ctx fu (\w -> lift $ f w es)
  where
    es = ctxPersonio ctx

withAuthUser'
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => a                           -- ^ Response to unauthenticated users
    -> Ctx
    -> auth
    -> (World -> LogT m a)
    -> LogT m a
withAuthUser' _def ctx _fu f = do
     world <- liftIO $ readTVarIO (ctxWorld ctx)
     f world

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "FUM Carbon"
    & serverDescription      .~ "FUM faster than ever"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp checklistApi .~ server
    & serverEnvPfx           .~ "FUMAPP"

makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
makeCtx Config {..} lgr _cache = do
    mgr <- newManager tlsManagerSettings
    employees <- Personio.evalPersonioReqIO mgr lgr cfgPersonioCfg Personio.PersonioEmployees
    ctx <- newCtx
        lgr
        employees
        cfgPostgresConnInfo
        emptyWorld
    pure (ctx, [])
