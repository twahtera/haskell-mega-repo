{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.FUM.Types.World (
    World,
    emptyWorld,
    validateWorld,
    -- * Lenses
    worldEmployees,
    worldCustomers,
    worldMailboxes,
    worldGroups,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.IdMap   (IdMap)

import Futurice.App.FUM.Types.Basic

-- import qualified Personio as P

-- | World desribes the state of the db.
data World = World
    { _worldEmployees  :: !(IdMap Employee)
    , _worldCustomers  :: !(IdMap Customer)
    , _worldMailboxes  :: !(IdMap Mailbox)
    , _worldGroups     :: !(IdMap Group)
    }

makeLenses ''World

emptyWorld :: World
emptyWorld = World mempty mempty mempty mempty

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | TODO: perform "GC"
validateWorld :: World -> World
validateWorld = id

-------------------------------------------------------------------------------
-- ToJSON / ToSchema
-------------------------------------------------------------------------------

-- TODO
