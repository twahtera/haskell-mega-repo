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

import qualified FUM (UserName (..))

server :: Ctx -> Server ChecklistAPI
server ctx = indexPageImpl ctx

indexPageImpl
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Location
    -> Maybe UUID
    -> Maybe UUID
    -> m (Page "indexpage")
indexPageImpl world fu loc cid tid = case userInfo of
    Nothing        -> pure nonAuthorizedPage
    Just userInfo' -> do
        today <- currentDay
        pure $ indexPage world today userInfo' loc checklist task
  where
    userInfo :: Maybe (FUM.UserName, TaskRole, Location)
    userInfo = world ^? worldUsers . ix fu . _Just

    checklist = do
        cid' <- cid
        world ^? worldLists . ix (Identifier cid')

    task = do
        tid' <- tid
        world ^? worldTasks . ix (Identifier tid')

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Checklist API"
    "Super TODO"
    (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    getConfig cfgPort
    checklistApi server futuriceNoMiddleware
    $ \cfg _cache -> do
        world0 <- generate (resize 200 arbitrary)
        let world1 = if cfgMockAuth cfg
            then world0 & worldUsers .~ const (Just mockCredentials)
            else world0
        pure world1
  where
    mockCredentials = (FUM.UserName "phadej", TaskRoleIT, LocHelsinki)
