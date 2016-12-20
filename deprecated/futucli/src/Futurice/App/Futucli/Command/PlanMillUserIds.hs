{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Futucli.Command.PlanMillUserIds (planMillUserIds) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Http          (evalHttpT)
import Text.Regex.Applicative.Text (anySym, match)

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as V

import qualified Control.Monad.PlanMill as PM
import qualified FUM
import qualified PlanMill               as PM

import Futurice.App.Futucli.Cfg

planMillUserIds :: Cfg -> IO ()
planMillUserIds cfg = withStderrLogger $ \logger -> do
    manager <- newManager tlsManagerSettings
    planmillUsers <- evalHttpT $ runLogT "futucli" logger $ flip runReaderT cfg $ PM.planmillAction PM.users
    fumUsers <- FUM.fetchList manager (_cfgFumToken cfg) (_cfgFumBaseurl cfg) (_cfgFumList cfg)
    process planmillUsers fumUsers
  where
    process :: Vector PM.User -> Vector FUM.User -> IO ()
    process = mapM_ . process'

    process' :: Vector PM.User -> FUM.User -> IO ()
    process' planmillUsers fumUser = case V.find p planmillUsers of
        Nothing           -> T.putStrLn $ fumUserName <> " - no match"
        Just planmillUser -> T.putStrLn $ fumUserName <> " : " <> T.pack (show $ planmillUser ^. PM.identifier)
      where
        fumUserName = fumUser ^. FUM.userName . FUM.getUserName
        fumUserNameStr = T.unpack fumUserName
        p planmillUser = case match ("https://login.futurice.com/openid/" *> many anySym) (PM.uUserName planmillUser) of
            Just name  -> name == fumUserNameStr
            Nothing    -> False
