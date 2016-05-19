-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Random utilities
module PlanMill.Internal (
    bsShow,
    tshow,
    utcTimeToInteger,
    ) where

import Data.ByteString       (ByteString)
import Data.String           (fromString)
import Data.Text             (Text)
import Data.Time             (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- | We /could/ encode to UTF8, but we don't.
bsShow :: Show a => a -> ByteString
bsShow = fromString . show

tshow :: Show a => a -> Text
tshow = fromString . show

utcTimeToInteger :: UTCTime -> Integer
utcTimeToInteger = round . utcTimeToPOSIXSeconds
