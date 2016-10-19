module PlanMill.Internal.Prelude (
    module Futurice.Prelude,
    module Futurice.Time,
    -- * aeson
    FromJSON(..), ToJSON(..),
    withObject, withText,
    object,
    (.:), (.:?), (.=), (.!=),
    Value(..),
    typeMismatch,
    -- * binary-tagged
    HasSemanticVersion, HasStructuralInfo(..), sopStructuralInfo,
    -- * Time related
    ZonedTime, UTCTime(..),
    Aeson.getU, Aeson.getZ, dayFromZ, zonedTimeDay,
    utcTimeToInteger,
    -- * Misc
    bsShow,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Time

import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText,
       (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types      (typeMismatch)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo (..), sopStructuralInfo)
import Data.ByteString       (ByteString)
import Data.Time             (ZonedTime, zonedTimeToLocalTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import qualified Data.Aeson.Extra   as Aeson
import qualified Data.Text.Encoding as TE

dayFromZ :: Aeson.Z -> Day
dayFromZ = zonedTimeDay . Aeson.getZ

zonedTimeDay :: ZonedTime -> Day
zonedTimeDay = localDay . zonedTimeToLocalTime

bsShow :: Show a => a -> ByteString
bsShow = TE.encodeUtf8 . textShow

utcTimeToInteger :: UTCTime -> Integer
utcTimeToInteger = round . utcTimeToPOSIXSeconds
