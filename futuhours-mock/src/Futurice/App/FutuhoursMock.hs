{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Servant

-- Contacts modules
import Futurice.App.FutuhoursMock.API
import Futurice.App.FutuhoursMock.Config (Config (..), getConfig)
import Futurice.App.FutuhoursMock.Logic  (projectEndpoint)
import Futurice.App.FutuhoursMock.Types

server :: Ctx -> Server FutuhoursAPI 
server ctx = pure "This is futuhours mock api"
    :<|> liftIO (projectEndpoint ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Futuhours MOCK api"
    "Is it real?"
    (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    getConfig cfgPort
    futuhoursApi server futuriceNoMiddleware
    $ \Config {..} _cache -> do
        pure Ctx
