{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.ResultInterval (
    ResultInterval(..),
    IntervalType(..),
    mkResultInterval,
    intervalToQueryString,
    -- * Interval
    Interval,
    mkInterval,
    mkIntervalSafe,
    singletonInterval,
    elimInterval,
    intervalMin,
    intervalMax,
    intervalDayToIntervalUTC,
    InvalidInterval(..),
    ) where

import PlanMill.Internal.Prelude
import Numeric.Interval.NonEmpty
import PlanMill.Types.Request    (QueryString)
import PlanMill.Types.UOffset    (showPlanmillUTCTime)

import qualified Data.Map as Map

-- | Interval field.
data IntervalType = IntervalStart
                  | IntervalFinish
                  | IntervalCreated
                  | IntervalModified
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)

-- | Map to query string value.
intervalToQueryString :: ResultInterval -> QueryString
intervalToQueryString (ResultInterval t i) =
    Map.fromList [ ("interval", t')
    , ("intervalstart", fromString . showPlanmillUTCTime $ inf i)
    , ("intervalfinish", fromString . showPlanmillUTCTime $ sup i)
    ]
  where
    t' = case t of
        IntervalStart    -> "start"
        IntervalFinish   -> "finish"
        IntervalCreated  -> "created"
        IntervalModified -> "modified"

-- | Result interval. Use 'mkResultInterval' to construct.
data ResultInterval = ResultInterval IntervalType (Interval UTCTime)
    deriving (Eq, Ord, Show, Generic, Typeable)

-- | Throws 'InvalidResultInterval' if @from@ and @to@ aren't ordered.
mkResultInterval
    :: MonadThrow m
    => IntervalType
    -> UTCTime       -- ^ from
    -> UTCTime       -- ^ to
    -> m ResultInterval
mkResultInterval i a b = ResultInterval i <$> mkInterval a b

-------------------------------------------------------------------------------
-- Interval compatibility
-------------------------------------------------------------------------------

mkInterval
    :: (MonadThrow m, Ord a, Show a, Typeable a)
    => a  -- ^ Lower bound
    -> a  -- ^ Upper bound
    -> m (Interval a)
mkInterval a b = maybe (throwM $ InvalidInterval a b) return $ interval a b

-- | '...'
mkIntervalSafe
    :: Ord a
    => a  -- ^ Lower bound
    -> a  -- ^ Upper bound
    -> Interval a
mkIntervalSafe = (...)
{-# DEPRECATED mkIntervalSafe "use (...)" #-}

-- | 'singleton'
singletonInterval :: a -> Interval a
singletonInterval = singleton
{-# DEPRECATED singletonInterval "use singleton" #-}

elimInterval :: (a -> a -> b) -> Interval a -> b
elimInterval f i = f (inf i) (sup i)

-- | 'inf'
intervalMin :: Interval a -> a
intervalMin = inf
{-# DEPRECATED intervalMin "use inf" #-}

-- | 'sup
intervalMax :: Interval a -> a
intervalMax = sup
{-# DEPRECATED intervalMax "use sup" #-}

data InvalidInterval a = InvalidInterval a a
    deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (InvalidInterval a)

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

intervalDayToIntervalUTC :: Interval Day -> Interval UTCTime
intervalDayToIntervalUTC i = a' ... b'
  where
    a' = UTCTime (inf i) 0
    b' = UTCTime (sup i) 0
