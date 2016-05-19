module PlanMill.Internal.Prelude (
    module Futurice.Prelude,
    -- * aeson
    FromJSON(..), ToJSON(..),
    withObject, withText,
    object,
    (.:), (.:?), (.=), (.!=),
    Value(..),
    U(..), Z(..),
    -- * binary-tagged
    HasSemanticVersion, HasStructuralInfo(..), sopStructuralInfo,
    -- * Time related
    ZonedTime, UTCTime(..),
    dayFromZ, zonedTimeDay,
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.Compat  (FromJSON (..), ToJSON (..), Value (..), object,
                           withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Extra   (U (..), Z (..))
import Data.Binary.Tagged (HasSemanticVersion, HasStructuralInfo (..),
                           sopStructuralInfo)
import Data.Time          (UTCTime (..), ZonedTime, localDay,
                           zonedTimeToLocalTime)

dayFromZ :: Z -> Day
dayFromZ = zonedTimeDay . getZ

zonedTimeDay :: ZonedTime -> Day
zonedTimeDay = localDay . zonedTimeToLocalTime
