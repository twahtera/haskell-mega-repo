{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | TODO: rename to MissingHours
module Futurice.App.FutuHours.Reports.MissingHours (
    missingHours,
    missingHoursForUser,
    ) where

import Futurice.Prelude
import Futurice.Time
import Prelude ()

import Data.Fixed (Centi)
import Data.Maybe (mapMaybe)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Vector         as V
import qualified PlanMill            as PM

import Futurice.App.FutuHours.PlanMillCache
import Futurice.App.FutuHours.Types
import Futurice.Report

missingHoursForUser
    :: (PM.MonadPlanMill m, MonadPlanMillCached m)
    => PM.Interval Day
    -> PM.UserId
    -> m (Per Employee (Vector MissingHour))
missingHoursForUser interval uid = do
    u <- PM.planmillAction $ PM.user uid
    t <- traverse (PM.planmillAction . PM.team) (PM.uTeam u)
    c <- PM.enumerationValue (PM.uContractType u) "Unknown Contract"
    uc <- PM.planmillVectorAction $ PM.userCapacity interval uid
    let uc' = capacities uc
    tr <- cachedTimereports interval uid
    let employee = Employee
            { employeeName     = PM.uFirstName u <> " " <> PM.uLastName u
            , employeeTeam     = maybe "Unknown Team" PM.tName t
            , employeeContract = c
            }
    let f (day, cap) =  MissingHour
            { missingHourDay      = day
            , missingHourCapacity = cap
            }
    let tr' = V.fromList $ map f $ Map.toList $ Map.differenceWith minus uc' $ reportedDays tr
    return $ Per employee tr'
  where
    -- For now show only days without any hour markings
    minus :: NDT 'Hours Centi -> NDT 'Hours Centi -> Maybe (NDT 'Hours Centi)
    minus a b
        | b > 0      = Nothing
        | otherwise  = Just a

    capacities :: PM.UserCapacities -> Map Day (NDT 'Hours Centi)
    capacities
        = Map.fromList
        . filter (isPositive . snd)
        . map (\x -> (PM.userCapacityDate x, ndtConvert' $ PM.userCapacityAmount x))
        . toList

    reportedDays :: PM.Timereports -> Map Day (NDT 'Hours Centi)
    reportedDays
        = Map.fromListWith (+)
        . map (\x -> (PM.trStart x, ndtConvert' $ PM.trAmount x))
        . toList

-- |
--
-- /TODO/
--
-- * Types
missingHours
    :: forall m f. (PM.MonadPlanMill m, MonadPlanMillCached m, Foldable f)
    => UTCTime
    -> PlanmillUserLookupTable
    -> PM.Interval Day
    -> f FUMUsername
    -> m MissingHoursReport
missingHours now pmUsers interval usernames = do
    rs <- fmap HM.fromList
        . f
        . mapMaybe g
        $ usernames'
    return $ Report (ReportGenerated now) rs
  where
    f :: [(k, PM.UserId)] -> m [(k, Per Employee (Vector MissingHour))]
    f = (traverse . traverse) (missingHoursForUser interval)

    usernames' :: [FUMUsername]
    usernames' = case toList usernames of
        [] -> HM.keys pmUsers
        us -> us

    g :: FUMUsername -> Maybe (FUMUsername, PM.UserId)
    g n = (,) n . (^.PM.identifier) <$> HM.lookup n pmUsers

isPositive :: (Num a, Ord a) => a -> Bool
isPositive = (>0)
