module Futurice.App.Smileys.Ctx (
    Ctx(..),
    ) where

import Prelude ()
import Futurice.Prelude

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)

import qualified FUM

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxPostgresPool :: !(Pool Connection)
    , ctxMockUser     :: !(Maybe FUM.UserName)
    }
