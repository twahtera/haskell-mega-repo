{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM    (atomically, readTVarIO, writeTVar)
import Data.Foldable             (foldl')
import Data.Pool                 (withResource)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant
import Servant

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Pages.Checklist
import Futurice.App.Checklist.Pages.Employee
import Futurice.App.Checklist.Pages.Error     (forbiddedPage, notFoundPage)
import Futurice.App.Checklist.Pages.Index
import Futurice.App.Checklist.Pages.Task
import Futurice.App.Checklist.Pages.Tasks
import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Types.Ctx

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM                        (UserName (..))

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Ctx -> Server ChecklistAPI
server ctx = indexPageImpl ctx
    :<|> tasksPageImpl ctx
    :<|> checklistPageImpl ctx
    :<|> taskPageImpl ctx
    :<|> employeePageImpl ctx

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Location
    -> Maybe (Identifier Checklist)
    -> Maybe (Identifier Task)
    -> m (HtmlPage "indexpage")
indexPageImpl ctx fu loc cid tid = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        today <- currentDay
        pure $ indexPage world today userInfo loc checklist task
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

        task = do
            tid' <- tid
            world ^? worldTasks . ix tid'

tasksPageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe TaskRole
    -> Maybe (Identifier Checklist)
    -> m (HtmlPage "tasks")
tasksPageImpl ctx fu role cid = withAuthUser ctx fu impl
  where
    impl world userInfo =
        pure $ tasksPage world userInfo role checklist
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

taskPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Identifier Task
    -> m (HtmlPage "task")
taskPageImpl ctx fu tid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldTasks . ix tid of
        Nothing   -> pure notFoundPage
        Just task -> do
            today <- currentDay
            pure $ taskPage world today userInfo task

checklistPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Identifier Checklist
    -> m (HtmlPage "checklist")
checklistPageImpl ctx fu cid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldLists . ix cid of
        Nothing        -> pure notFoundPage
        Just checklist -> do
            today <- currentDay
            pure $ checklistPage world today userInfo checklist

employeePageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Identifier Employee
    -> m (HtmlPage "employee")
employeePageImpl ctx fu eid = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ case world ^? worldEmployees . ix eid of
        Nothing       -> notFoundPage
        Just employee -> employeePage world userInfo employee

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: MonadIO m
    => Ctx -> Maybe FUM.UserName
    -> (World -> AuthUser -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser ctx fu f = do
    world <- liftIO $ readTVarIO $ ctxWorld ctx
    case userInfo world of
        Nothing        -> pure forbiddedPage
        Just userInfo' -> f world userInfo'
  where
    userInfo :: World -> Maybe (FUM.UserName, TaskRole, Location)
    userInfo world = world ^? worldUsers . ix fu . _Just

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "Checklist"
    & serverDescription      .~ "Super TODO"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp checklistApi .~ server
    & serverEnvPfx           .~ "CHECKLISTAPP"
  where
    mockCredentials = (FUM.UserName "phadej", TaskRoleIT, LocHelsinki)

    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx cfg _logger _cache = do
        ctx <- newCtx (cfgPostgresConnInfo cfg) emptyWorld
        cmds <- withResource (ctxPostgres ctx) $ \conn ->
            Postgres.fromOnly <$$> Postgres.query_ conn "SELECT cmddata FROM checklist2.commands ORDER BY cid;"
        let world0 = foldl' (flip applyCommand) emptyWorld cmds
        let world1 = if cfgMockAuth cfg
            then world0 & worldUsers .~ const (Just mockCredentials)
            else world0
        atomically $ writeTVar (ctxWorld ctx) world1
        pure ctx
