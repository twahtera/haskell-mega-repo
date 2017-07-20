module Futurice.App.Smileys.Ctx (
    Ctx(..),
    ) where

import Futurice.Cache   (DynMapCache)
import Futurice.Prelude
import Prelude ()

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)

import qualified FUM

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxPostgresPool :: !(Pool Connection)
    , ctxCache        :: !DynMapCache
    , ctxLogger       :: !Logger
    , ctxMockUser     :: !(Maybe FUM.UserName)
    }
