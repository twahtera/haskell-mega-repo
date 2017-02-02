{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Checklist (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM    (atomically, readTVarIO, writeTVar)
import Data.Foldable             (foldl')
import Data.Pool                 (withResource)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant
import Futurice.Stricter
import Servant

import Futurice.App.Checklist.Ack
import Futurice.App.Checklist.API
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Pages.Checklist
import Futurice.App.Checklist.Pages.Checklists
import Futurice.App.Checklist.Pages.CreateChecklist
import Futurice.App.Checklist.Pages.CreateEmployee
import Futurice.App.Checklist.Pages.CreateTask
import Futurice.App.Checklist.Pages.Employee
import Futurice.App.Checklist.Pages.Error
       (forbiddedPage, notFoundPage)
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
    :<|> checklistsPageImpl ctx
    :<|> createChecklistPageImpl ctx
    :<|> createTaskPageImpl ctx
    :<|> createEmployeePageImpl ctx
    :<|> checklistPageImpl ctx
    :<|> taskPageImpl ctx
    :<|> employeePageImpl ctx
    :<|> commandImpl ctx

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe Location
    -> Maybe (Identifier Checklist)
    -> Maybe (Identifier Task)
    -> Bool
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx fu loc cid tid showAll = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        today <- currentDay
        pure $ indexPage world today userInfo loc checklist task showAll
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

        task = do
            tid' <- tid
            world ^? worldTasks . ix tid'

tasksPageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe TaskRole
    -> Maybe (Identifier Checklist)
    -> Handler (HtmlPage "tasks")
tasksPageImpl ctx fu role cid = withAuthUser ctx fu impl
  where
    impl world userInfo =
        pure $ tasksPage world userInfo role checklist
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

createChecklistPageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler (HtmlPage "create-checklist")
createChecklistPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createChecklistPage world userInfo

createTaskPageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler (HtmlPage "create-task")
createTaskPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createTaskPage world userInfo

createEmployeePageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe (Identifier Employee)
    -> Handler (HtmlPage "create-employee")
createEmployeePageImpl ctx fu meid = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createEmployeePage world userInfo memployee
      where
        memployee = meid >>= \eid -> world ^? worldEmployees . ix eid


checklistsPageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler (HtmlPage "checklists")
checklistsPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ checklistsPage world userInfo

taskPageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Identifier Task
    -> Handler (HtmlPage "task")
taskPageImpl ctx fu tid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldTasks . ix tid of
        Nothing   -> pure notFoundPage
        Just task -> do
            today <- currentDay
            pure $ taskPage world today userInfo task

checklistPageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Identifier Checklist
    -> Handler (HtmlPage "checklist")
checklistPageImpl ctx fu cid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldLists . ix cid of
        Nothing        -> pure notFoundPage
        Just checklist -> do
            today <- currentDay
            pure $ checklistPage world today userInfo checklist

employeePageImpl
    :: Ctx
    -> Maybe FUM.UserName
    -> Identifier Employee
    -> Handler (HtmlPage "employee")
employeePageImpl ctx fu eid = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ case world ^? worldEmployees . ix eid of
        Nothing       -> notFoundPage
        Just employee -> employeePage world userInfo employee

-------------------------------------------------------------------------------
-- Command implementation
-------------------------------------------------------------------------------

commandImpl
    :: (MonadIO m, MonadBaseControl IO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Command Proxy
    -> m Ack
commandImpl ctx fu cmd = runLogT "command" (ctxLogger ctx) $
    withAuthUser' (AckErr "forbidden") ctx fu $ \_world (fumUsername, _) -> do
        (cmd', res) <- instantiatedCmd
        ctxApplyCmd fumUsername cmd' ctx
        pure res
  where
    instantiatedCmd = flip runStricterT mempty $ traverseCommand genIdentifier cmd

    genIdentifier
        :: (MonadIO m, MonadWriter Ack m)
        => CIT x -> Proxy (Identifier x) -> m (Identity (Identifier x))
    genIdentifier CITEmployee Proxy = do
        eid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ toUrlPiece $
            safeLink checklistApi employeePageEndpoint eid
        pure (Identity eid)
    genIdentifier CITTask Proxy = do
        tid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ toUrlPiece $
            safeLink checklistApi taskPageEndpoint tid
        pure (Identity tid)
    genIdentifier CITChecklist Proxy = do
        cid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ toUrlPiece $
            safeLink checklistApi checklistPageEndpoint cid
        pure (Identity cid)

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => Ctx -> Maybe FUM.UserName
    -> (World -> AuthUser -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser ctx fu f = runLogT "withAuthUser" (ctxLogger ctx) $
    withAuthUser' forbiddedPage ctx fu (\w u -> lift $ f w u)

withAuthUser'
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => a                           -- ^ Response to unauthenticated users
    -> Ctx
    -> Maybe FUM.UserName
    -> (World -> AuthUser -> LogT m a)
    -> LogT m a
withAuthUser' def ctx fu f = do
    let fu'      = fu <|> ctxMockUser ctx
        authUser = fu' >>= \fu'' -> (fu'',) <$> ctxACL ctx ^. at fu''
    case authUser of
        Nothing -> do
            logInfo_ $ "Unauthorised user " <> textShow fu
            pure def
        Just authUser' -> do
            world <- liftIO $ readTVarIO (ctxWorld ctx)
            f world authUser'

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
    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx Config {..} logger _cache = do
        ctx <- newCtx
            logger
            cfgPostgresConnInfo
            cfgFumToken cfgFumBaseurl
            (cfgFumITGroup, cfgFumHRGroup, cfgFumSupervisorGroup)
            cfgMockUser
            emptyWorld
        cmds <- withResource (ctxPostgres ctx) $ \conn ->
            Postgres.fromOnly <$$> Postgres.query_ conn "SELECT cmddata FROM checklist2.commands ORDER BY cid;"
        let world0 = foldl' (flip applyCommand) emptyWorld cmds
        atomically $ writeTVar (ctxWorld ctx) world0
        pure ctx
