{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.GitHubProxy.Types (
    Ctx (..),
    ) where

import Prelude ()
import Control.Monad.Logger       (LogLevel (..))
import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.Servant           (DynMapCache)
import GitHub.Auth                   (Auth)

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxCache        :: !DynMapCache
    , ctxGitHubAuth   :: !Auth
    , ctxPostgresPool :: !(Pool Connection)  -- TODO: write a lib to handle these
    , ctxLogLevel     :: !LogLevel
    }
