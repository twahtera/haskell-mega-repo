{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- TODO: remove?
module PlanMill.Types.UOffset where

import PlanMill.Internal.Prelude

import Data.Time               (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)

-- | Hopefully temporaty object to work around over strict PlanMill API
newtype UOffset = UOffset { getUOffset :: UTCTime }

instance ToJSON UOffset where
    toJSON = String . fromString . showPlanmillUTCTime . getUOffset

showPlanmillUTCTime :: UTCTime -> String
showPlanmillUTCTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000+0000"

showPlanmillDay :: Day -> String
showPlanmillDay d = showPlanmillUTCTime (UTCTime d 0)
