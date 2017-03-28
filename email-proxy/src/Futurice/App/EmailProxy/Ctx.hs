module Futurice.App.EmailProxy.Ctx where

import Prelude ()
import Futurice.Prelude

import Futurice.App.EmailProxy.Config

data Ctx = Ctx
    { ctxLogger  :: !Logger
    , ctxConfig  :: !Config
    , ctxManager :: !Manager
    }
