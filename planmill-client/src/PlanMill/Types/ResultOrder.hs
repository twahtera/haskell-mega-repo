{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
module PlanMill.Types.ResultOrder (ResultOrder(..)) where

import PlanMill.Internal.Prelude
import Prelude                   ()

data ResultOrder = Ascending | Descending
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)
