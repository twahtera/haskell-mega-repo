{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.GitHubProxy.Types (
    Ctx (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.PostgresPool
import Futurice.Servant           (DynMapCache)
import GitHub.Auth                (Auth)

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxCache        :: !DynMapCache
    , ctxGitHubAuth   :: !Auth
    , ctxPostgresPool :: !(Pool Connection)  -- TODO: write a lib to handle these
    , ctxLogger       :: !Logger
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgresPool
