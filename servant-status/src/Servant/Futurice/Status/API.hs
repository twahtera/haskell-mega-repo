{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
module Servant.Futurice.Status.API (
    StatusAPI,
    statusAPI,
    serveStatus,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text)
import Servant

import Servant.Futurice.Status.Types

type StatusAPI
    = "status" :> Get '[PlainText] Text
    :<|> "status.json" :> Get '[JSON] StatusInfo

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

serveStatus :: StatusInfoIO -> Server StatusAPI
serveStatus (SIIO stats) = liftIO (fmap statusInfoToText stats) :<|> liftIO stats 