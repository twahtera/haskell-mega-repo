-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.MaybeNumberText (
    MaybeNumberText(..),
    ) where

import PlanMill.Internal.Prelude
import Prelude                   ()

-- | Sometimes textual fields are numbers.
--
-- TODO: Hopefully this is temporary hack
newtype MaybeNumberText = MaybeNumberText { getMaybeNumberText :: Text }

instance FromJSON MaybeNumberText where
    parseJSON (String t) = pure . MaybeNumberText $ t
    parseJSON (Number n) = pure . MaybeNumberText . view packed . show $ n
    parseJSON _          = fail "Not string or number"
