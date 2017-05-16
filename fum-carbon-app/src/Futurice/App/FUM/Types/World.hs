{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.FUM.Types.World (
    World,
    emptyWorld,
    mkWorld,
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

-- | Create a world.
--
-- /TODO/:
--
-- * validate input
--
mkWorld
    :: IdMap Employee
    -> IdMap Customer
    -> IdMap Mailbox
    -> IdMap Group
    -> World -- ^ todo make Either
mkWorld = World

emptyWorld :: World
emptyWorld = mkWorld mempty mempty mempty mempty
