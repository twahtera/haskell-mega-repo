module Futurice.App.SmsProxy.Ctx where

import Prelude ()
import Futurice.Prelude

import Futurice.App.SmsProxy.Config

data Ctx = Ctx
    { ctxLogger :: Logger
    , ctxConfig :: Config
    }
