-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
module PlanMill.Types.ResultOrder (ResultOrder(..)) where

import PlanMill.Internal.Prelude

data ResultOrder = Ascending | Descending
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)
