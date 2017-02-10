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
    -- ** extras
    getParsedAsText, getParsedAsIntegral,
    -- * binary-tagged
    HasSemanticVersion, HasStructuralInfo(..), sopStructuralInfo,
    -- * intervals
    Interval, (...), inf, sup,
    -- * Time related
    ZonedTime, UTCTime(..),
    Aeson.getU, Aeson.getZ, dayFromZ, zonedTimeDay,
    utcTimeToInteger,
    -- * HasKey
    HasKey (..),
    -- * Misc
    bsShow,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText,
       (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types          (typeMismatch)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo (..), sopStructuralInfo)
import Data.Time                 (ZonedTime, zonedTimeToLocalTime)
import Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import Futurice.Aeson            (getParsedAsIntegral, getParsedAsText)
import Futurice.IdMap            (HasKey (..))
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))

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
