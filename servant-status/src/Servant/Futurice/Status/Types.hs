{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Servant.Futurice.Status.Types (
    Status(..),
    StatusMetric(..),
    StatusKey,
    StatusInfo,
    StatusInfoIO(..),
    getStatusInfo,
    -- * Status smart constructors
    status,
    group,
    metric,
    info,
    statusIO,
    metricIO,
    infoIO,
    -- * Human readable format
    statusInfoToText,
    -- * Internal
    calculateStatus,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Applicative (liftA2)
import Data.Aeson.Compat
import Data.Hashable       (Hashable)
import Data.Int            (Int32, Int64)
import Data.List           (sortBy)
import Data.Ord            (comparing)
import Data.Semigroup      (Semigroup (..))
import Data.String         (IsString (..))
import Data.Text           (Text)
import Data.Typeable       (Typeable)
import Data.Word           (Word32, Word64)
import GHC.Generics        (Generic)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

-- | Status of the system.
data Status = StatusOk
            | StatusWarn !Text
            | StatusError !Text
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Semigroup Status where
    x@(StatusError _) <> _ = x
    x@(StatusWarn _)  <> _ = x
    _                 <> y = y

instance Monoid Status where
    mempty = StatusOk
    mappend = (<>)

-- | Some informative metric of the system.
data StatusMetric = StatusCounterW !Word64
                  | StatusCounterI !Int64
                  | StatusCounterD !Double
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

class ToStatusMetric  a where
    toStatusMetric :: a -> StatusMetric

instance ToStatusMetric StatusMetric where
    toStatusMetric = id

instance ToStatusMetric Word64 where
    toStatusMetric = StatusCounterW

instance ToStatusMetric Int64 where
    toStatusMetric = StatusCounterI

instance ToStatusMetric Double where
    toStatusMetric = StatusCounterD

instance ToStatusMetric Int where
    toStatusMetric = StatusCounterI . fromIntegral

instance ToStatusMetric Word where
    toStatusMetric = StatusCounterW . fromIntegral

instance ToStatusMetric Int32 where
    toStatusMetric = StatusCounterI . fromIntegral

instance ToStatusMetric Word32 where
    toStatusMetric = StatusCounterW . fromIntegral

-- | Status key is an non-empty list of text strings.
newtype StatusKey = StatusKey Text
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable StatusKey
instance IsString StatusKey where
    fromString = StatusKey . T.pack

-- | Status map, collection of 'Status' and 'StatusMetric
data StatusInfo
    = SI !Status !(HM.HashMap StatusKey StatusInfo)
    | SM !StatusMetric
    | ST !Text           -- ^ Info message
    deriving (Eq, Show, Generic, Typeable)

instance Semigroup StatusInfo where
    SI StatusOk a' <> x
        | HM.null a'   = x
    SI a a' <> SI b b' = SI (a <> b) (a' <> b')
    SI a a' <> _       = SI a a'
    _       <> SI b b' = SI b b'
    -- metric are higher priority
    x@SM {} <> _       = x
    _       <> x@SM {} = x
    x@ST {} <> _       = x

instance Monoid StatusInfo where
    mempty  = SI StatusOk HM.empty
    mappend = (<>)

-- | We have to read status of the system
newtype StatusInfoIO = SIIO (IO StatusInfo)

instance Semigroup StatusInfoIO where
    SIIO a <> SIIO b = SIIO $ liftA2 (<>) a b

instance Monoid StatusInfoIO where
    mempty  = SIIO (pure mempty)
    mappend = (<>) 

getStatusInfo :: StatusInfoIO -> IO StatusInfo
getStatusInfo (SIIO s) = s

------------------------------------------------------------------------------
-- Status smart constructors
------------------------------------------------------------------------------

group :: StatusKey -> StatusInfo -> StatusInfo
group k s = SI (calculateStatus s) . HM.singleton k $ s

statusIO :: StatusKey -> IO Status -> StatusInfoIO
statusIO k = SIIO . fmap (status k)

status :: StatusKey -> Status -> StatusInfo
status k  = group k . flip SI mempty

metricIO :: ToStatusMetric metric => StatusKey -> IO metric -> StatusInfoIO
metricIO k = SIIO . fmap (metric k)

metric :: ToStatusMetric metric => StatusKey -> metric -> StatusInfo
metric k = group k . SM . toStatusMetric

info :: StatusKey -> Text -> StatusInfo
info k = group k . ST

infoIO :: StatusKey -> IO Text -> StatusInfoIO
infoIO k = SIIO . fmap (info k)

------------------------------------------------------------------------------
-- Aeson instance for StatusInfo
------------------------------------------------------------------------------

statusToText :: Status -> Text
statusToText StatusOk        = "OK"
statusToText (StatusWarn t)  = "WARN " <> t
statusToText (StatusError t) = "ERROR " <> t

instance ToJSON Status where
    toJSON  = String . statusToText

instance ToJSON StatusMetric where
    toJSON (StatusCounterW c) = toJSON c
    toJSON (StatusCounterI c) = toJSON c
    toJSON (StatusCounterD c) = toJSON c

flattenKey :: StatusKey -> Text
flattenKey (StatusKey k) = k

instance ToJSON StatusInfo where
    toJSON (SI s m) = object $ ("status" .= toJSON s) : (fmap f $ HM.toList m)
      where f (k, v) = flattenKey k .= toJSON v
    toJSON (SM m)   = toJSON m
    toJSON (ST t)   = toJSON t

------------------------------------------------------------------------------
-- Human readable format
------------------------------------------------------------------------------

statusInfoToText :: StatusInfo -> Text
statusInfoToText = T.unlines . go ""
  where
    go pfx (SM (StatusCounterW n)) = [pfx <> ": " <> T.pack (show n)]
    go pfx (SM (StatusCounterI n)) = [pfx <> ": " <> T.pack (show n)]
    go pfx (SM (StatusCounterD n)) = [pfx <> ": " <> T.pack (show n)]
    go pfx (ST t)                  = [pfx <> ": " <> t]
    go pfx (SI s sub)              =
        [subPfx "status" pfx <> ": " <> statusToText s] ++
        concatMap (\(k, v) -> go (subPfx k pfx) v) (sortBy (comparing fst) $ HM.toList sub)

    subPfx :: StatusKey -> Text -> Text
    subPfx (StatusKey k) pfx | T.null pfx = k
                             | otherwise  = pfx <> "." <> k

------------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------------

calculateStatus :: StatusInfo -> Status
calculateStatus (SI s _) = s
calculateStatus (SM _)   = mempty
calculateStatus (ST _)   = mempty
