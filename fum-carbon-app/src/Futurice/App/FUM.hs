{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM (defaultMain) where

import Control.Concurrent.STM    (atomically, readTVar, readTVarIO)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Futurice.Lomake
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Config
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Pages.CreateEmployee
import Futurice.App.FUM.Pages.Index
import Futurice.App.FUM.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

apiServer :: Ctx -> Server FumCarbonMachineApi
apiServer ctx = rawEmployees
  where
    -- TODO: eventually move to the Logic module
    rawEmployees = do
        today <- currentDay
        es <- liftIO $ readTVarIO $ ctxPersonio ctx
        pure $ filter (isCurrentEmployee today) $ toList es

    isCurrentEmployee today e =
        maybe True (today <=) (e ^. Personio.employeeEndDate) &&
        maybe False (<= today) (e ^. Personio.employeeHireDate)

-- TODO: write command handler
commandServer :: Ctx -> Server FumCarbonCommandApi
commandServer ctx (LomakeRequest cmd) = runLogT "command" (ctxLogger ctx) $ do
    logTrace "input" cmd
    fail "not-implemented"
    -- pure LomakeResponseNoop

server :: Ctx -> Server FumCarbonApi
server ctx = indexPageImpl ctx
    :<|> createEmployeePageImpl ctx
    :<|> commandServer ctx
    :<|> apiServer ctx

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> a
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world es = pure $ indexPage world es

createEmployeePageImpl
    :: Ctx
    -> a
    -> Personio.EmployeeId
    -> Handler (HtmlPage "create-employee")
createEmployeePageImpl ctx fu pid = withAuthUser ctx fu impl
  where
    impl world es = case es ^? ix pid of
        Just e -> pure $ createEmployeePage world es e
        Nothing -> pure notFoundPage

notFoundPage :: HtmlPage sym
notFoundPage = fumPage_ "Not found" ()
    ":("

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => Ctx
    -> auth
    -> (World -> IdMap.IdMap Personio.Employee -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser ctx fu f = runLogT "withAuthUser" (ctxLogger ctx) $
    withAuthUser' (error "forbiddenPage") ctx fu (\w es -> lift $ f w es)

withAuthUser'
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => a                           -- ^ Response to unauthenticated users
    -> Ctx
    -> auth
    -> (World -> IdMap.IdMap Personio.Employee -> LogT m a)
    -> LogT m a
withAuthUser' _def ctx _fu f = do
     (world, es) <- liftIO $ atomically $ (,)
          <$> readTVar (ctxWorld ctx)
          <*> readTVar (ctxPersonio ctx)
     f world es

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "FUM Carbon"
    & serverDescription      .~ "FUM faster than ever"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp fumCarbonApi .~ server
    & serverEnvPfx           .~ "FUMAPP"

makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
makeCtx Config {..} lgr _cache = do
    mgr <- newManager tlsManagerSettings

    -- employees
    let fetchEmployees = Personio.evalPersonioReqIO mgr lgr cfgPersonioCfg Personio.PersonioEmployees
    employees <- fetchEmployees

    -- context
    ctx <- newCtx
        lgr
        cfgPostgresConnInfo
        (IdMap.fromFoldable employees)
        emptyWorld

    -- jobs
    let employeesJob = mkJob "Update personio data" fetchEmployees $ tail $ every 300

    pure (ctx, [ employeesJob ])
