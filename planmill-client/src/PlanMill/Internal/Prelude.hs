module PlanMill.Internal.Prelude (
    module Futurice.Prelude,
    -- * aeson
    FromJSON(..), ToJSON(..),
    withObject, withText,
    object,
    (.:), (.:?), (.=), (.!=),
    Value(..),
    U(..), Z(..),
    typeMismatch,
    -- * binary-tagged
    HasSemanticVersion, HasStructuralInfo(..), sopStructuralInfo,
    -- * Time related
    ZonedTime, UTCTime(..),
    dayFromZ, zonedTimeDay,
    utcTimeToInteger,
    -- * Misc
    bsShow,
    ) where

import Futurice.Prelude
import Prelude ()

import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText,
       (.!=), (.:), (.:?), (.=))
import Data.Aeson.Extra      (U (..), Z (..))
import Data.Aeson.Types      (typeMismatch)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo (..), sopStructuralInfo)
import Data.ByteString       (ByteString)
import Data.Time
       (ZonedTime, zonedTimeToLocalTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import qualified Data.Text.Encoding as TE

dayFromZ :: Z -> Day
dayFromZ = zonedTimeDay . getZ

zonedTimeDay :: ZonedTime -> Day
zonedTimeDay = localDay . zonedTimeToLocalTime

bsShow :: Show a => a -> ByteString
bsShow = TE.encodeUtf8 . textShow

utcTimeToInteger :: UTCTime -> Integer
utcTimeToInteger = round . utcTimeToPOSIXSeconds
