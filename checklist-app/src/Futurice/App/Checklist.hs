{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Servant
import Test.QuickCheck  (arbitrary, generate, resize)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types
import Futurice.Lucid.Foundation     (HtmlPage)

import qualified FUM (UserName (..))

server :: Ctx -> Server ChecklistAPI
server ctx = indexPageImpl ctx
    :<|> tasksPageImpl ctx

indexPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Location
    -> Maybe UUID
    -> Maybe UUID
    -> m (HtmlPage "indexpage")
indexPageImpl ctx fu loc cid tid = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        today <- currentDay
        pure $ indexPage world today userInfo loc checklist task
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix (Identifier cid')

        task = do
            tid' <- tid
            world ^? worldTasks . ix (Identifier tid')

tasksPageImpl
    :: (MonadIO m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe TaskRole
    -> Maybe UUID
    -> m (HtmlPage "tasks")
tasksPageImpl ctx fu role cid = withAuthUser ctx fu impl
  where
    impl world userInfo =
        pure $ tasksPage world userInfo role checklist
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix (Identifier cid')

-- | Read only pages
withAuthUser
    :: MonadIO m
    => Ctx -> Maybe FUM.UserName
    -> (World -> AuthUser -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser ctx fu f = case userInfo of
    Nothing        -> pure nonAuthorizedPage
    Just userInfo' -> f ctx userInfo'
  where
    userInfo :: Maybe (FUM.UserName, TaskRole, Location)
    userInfo = ctx ^? worldUsers . ix fu . _Just

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "Checklist API"
    & serverDescription      .~ "Super TODO"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp checklistApi .~ server
  where
    mockCredentials = (FUM.UserName "phadej", TaskRoleIT, LocHelsinki)

    makeCtx :: Config -> DynMapCache -> IO Ctx
    makeCtx cfg _cache = do
        world0 <- generate (resize 200 arbitrary)
        let world1 = if cfgMockAuth cfg
            then world0 & worldUsers .~ const (Just mockCredentials)
            else world0
        pure world1
