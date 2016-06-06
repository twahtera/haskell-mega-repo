{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.CapacityCalendar (
    CapacityCalendar(..),
    CapacityCalendars,
    CapacityCalendarId,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Identifier (Identifier)

type CapacityCalendarId = Identifier CapacityCalendar
type CapacityCalendars = Vector CapacityCalendar

data CapacityCalendar = CapacityCalendar
    { ccCountry               :: !(Maybe Int)
    , ccDefaultWeekStartDay   :: !(Maybe Int)
    , ccDefaultDailyWorktime  :: !(Maybe Int)
    , ccDefaultDailyStartTime :: !(Maybe Int)
    , ccName                  :: !(Maybe Text)
    , ccStart                 :: !UTCTime
    , ccDefaultWeeklyWorkdays :: !(Maybe Int)
    , ccFinish                :: !UTCTime
    , ccActiveUsers           :: !(Maybe Int)
    , ccId                    :: !CapacityCalendarId
    , ccType                  :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''CapacityCalendar

instance Hashable CapacityCalendar
instance NFData CapacityCalendar
instance AnsiPretty CapacityCalendar
instance Binary CapacityCalendar
instance HasStructuralInfo CapacityCalendar where structuralInfo = sopStructuralInfo
instance HasSemanticVersion CapacityCalendar

instance FromJSON CapacityCalendar where
    parseJSON = withObject "CapacityCalendar" $ \obj ->
        CapacityCalendar <$> obj .: "country"
                         <*> obj .: "defaultWeekStartDay"
                         <*> obj .: "defaultDailyWorktime"
                         <*> obj .: "defaultDailyStartTime"
                         <*> obj .: "name"
                         <*> (getU <$> obj .: "start")
                         <*> obj .: "defaultWeeklyWorkdays"
                         <*> (getU <$> obj .: "finish")
                         <*> obj .: "activeUsers"
                         <*> obj .: "id"
                         <*> obj .: "type"
