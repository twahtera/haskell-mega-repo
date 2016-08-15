{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Lucid.Foundation.Futurice
import Futurice.Servant
import Servant
import Lucid hiding (for_)
import Test.QuickCheck (sample', arbitrary)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Types.Page

-- TODO: make to .Types.Ctx
type Ctx = ()

server :: Ctx -> Server ChecklistAPI
server _ = liftIO indexPage

indexPage :: IO (Page "indexpage")
indexPage = do
    users <- sample' arbitrary
    pure $ Page $ page_ "Checklist" $ ul_ $ for_ users $ \user ->
        li_ $ toHtml $ (show :: User -> String) user

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Checklist API"
    "Super TODO"
    (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    (pure ()) (const 8000) -- getConfig cfgPort
    checklistApi server futuriceNoMiddleware
    $ \_ _cache -> -- do
        pure ()
