{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Timereport (
    Timereport(..),
    Timereports,
    TimereportId,
    NewTimereport(..),
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Identifier      (HasIdentifier (..), Identifier)
import PlanMill.Types.MaybeNumberText (getMaybeNumberText)
import PlanMill.Types.Project         (ProjectId)
import PlanMill.Types.Task            (TaskId)
import PlanMill.Types.UOffset         (UOffset (..))
import PlanMill.Types.User            (UserId)

type TimereportId = Identifier Timereport
type Timereports = Vector Timereport

data Timereport = Timereport
    { _trId             :: !TimereportId
    , trTask            :: !TaskId
    , trAmount          :: !Double
    , trBillableStatus  :: !Int          -- TODO: make type
    , trBillingComment  :: !(Maybe Text)
    , trComment         :: !(Maybe Text)
    , trDutyType        :: !(Maybe Int)  -- TODO: make type, schema says it's mandatory
    , trFinish          :: !Day
    , trOvertimeAmount  :: !(Maybe Int)
    , trOvertimeComment :: !(Maybe Text)
    , trPerson          :: !UserId
    , trProject         :: !(Maybe ProjectId)
    , trStart           :: !Day   -- ^ The ''UTCTime' would be more precise, but we care about day more
    , trStatus          :: !Int   -- TODO: make type
    , trTravelAmount    :: !(Maybe Int)
    , trTravelComment   :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Timereport
deriveGeneric ''Timereport

instance HasIdentifier Timereport Timereport where
    identifier = trId

instance Hashable Timereport
instance NFData Timereport
instance AnsiPretty Timereport
instance Binary Timereport
instance HasStructuralInfo Timereport where structuralInfo = sopStructuralInfo
instance HasSemanticVersion Timereport

instance FromJSON Timereport where
    parseJSON = withObject "Timereport" $ \obj ->
        Timereport <$> obj .: "id"
                   <*> obj .: "task"
                   <*> obj .: "amount"
                   <*> obj .: "billableStatus"
                   <*> obj .:? "billingComment"
                   <*> (getMaybeNumberText <$$> obj .:? "comment")
                   <*> obj .:? "dutyType"
                   <*> (dayFromZ <$> obj .: "finish")
                   <*> obj .: "overtimeAmount"
                   <*> obj .: "overtimeComment"
                   <*> obj .: "person"
                   <*> obj .: "project"
                   <*> (dayFromZ <$> obj .: "start")
                   <*> obj .: "status"
                   <*> obj .: "travelAmount"
                   <*> obj .: "travelComment"

-- | Type used to create new timereports
--
-- TODO: Add UserId
data NewTimereport = NewTimereport
    { ntrTask    :: !TaskId
    , ntrStart   :: !Day
    , ntrAmount  :: !Int
    , ntrComment :: !Text
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''NewTimereport

instance Hashable NewTimereport
instance NFData NewTimereport
instance AnsiPretty NewTimereport
instance Binary NewTimereport
instance HasStructuralInfo NewTimereport where structuralInfo = sopStructuralInfo
instance HasSemanticVersion NewTimereport

instance ToJSON NewTimereport where
    toJSON NewTimereport {..} = object
        [ "task"    .= ntrTask
        , "start"   .= UOffset (UTCTime ntrStart 0)
        , "amount"  .= ntrAmount
        , "comment" .= ntrComment
        ]
