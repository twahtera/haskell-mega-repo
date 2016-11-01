{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances, UndecidableInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.TimereportsByTask (
    -- * Report
    TimereportsByTaskReport,
    timereportsByTaskReport,
    -- * Types
    TimereportsByTask (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Either               (partitionEithers)
import Data.Fixed                (Centi)
import Futurice.Generics
import Futurice.List
import Futurice.Integrations
import Futurice.Report.Columns
import Futurice.Time
import Numeric.Interval.NonEmpty ((...))

import qualified Data.List.NonEmpty as NE

import qualified Data.Map.Strict  as Map
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data TimereportsStats = TimereportsStats
    { _tsHours  :: !(NDT 'Hours Centi)
    , _tsCount  :: !Int
    , _tsMedian :: !(NDT 'Hours Centi)
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''TimereportsStats

instance NFData TimereportsStats
instance ToColumns TimereportsStats
instance ToSchema TimereportsStats where declareNamedSchema = sopDeclareNamedSchema
instance ToJSON TimereportsStats where
    toJSON = sopToJSON
    toEncoding = sopToEncoding


data TimereportsByTask = TimereportsByTask
    { _tbtProjectName    :: !Text
    , _tbtTaskName       :: !Text
    , _tbtHoursPrevMonth :: !TimereportsStats
    , _tbtHoursCurrMonth :: !TimereportsStats
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

-- makeLenses ''TimereportsByTask
deriveGeneric ''TimereportsByTask

instance NFData TimereportsByTask
instance ToSchema TimereportsByTask where declareNamedSchema = sopDeclareNamedSchema
instance ToJSON TimereportsByTask where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToColumns TimereportsByTask where
    type Columns TimereportsByTask =
        Append '[Text, Text] (Append (Columns TimereportsStats) (Columns TimereportsStats))

    columnNames _ =
        K "project-name" :*
        K "task-name" :*
        K "prev-hours" :*
        K "prev-count" :*
        K "prev-median" :*
        K "curr-hours" :*
        K "curr-count" :*
        K "curr-median" :*
        Nil

    toColumns (TimereportsByTask p t prev curr) =
        f <$> toColumns prev <*> toColumns curr
      where
        f prev' curr' = append (I p :* I t :* Nil) (append prev' curr')

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type TimereportsByTaskReport = Report
    "Hour marked by project/task"
    ReportGenerated
    [TimereportsByTask]

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

timereportsByTaskReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m TimereportsByTaskReport
timereportsByTaskReport = do
    now   <- currentTime
    today <- currentDay
    -- TODO: write date (not time!) handling lib
    let startDay = beginningOfPrevMonth today
    let midDay   = beginningOfCurrMonth today
    let endDay   = today
    let interval = startDay ... endDay
    -- Users
    fpm <- fumPlanmillMap
    -- Timereports
    trs <- concatMap toList <$>
        traverse (PMQ.timereports interval . view PM.identifier) (toList fpm)
    -- Group by task
    let trs1 = groupTimereports trs
    -- Group and sum
    trs2 <- itraverse (timereportsByTask midDay) trs1
    -- Sort
    let trs3 = sortBy (flip compare `on` _tbtHoursPrevMonth) $ toList trs2
    -- Result
    pure $ Report (ReportGenerated now) trs3

groupTimereports :: [PM.Timereport] -> Map PM.TaskId (NE.NonEmpty PM.Timereport)
groupTimereports = Map.unionsWith (<>) . map f
  where
    f tr = Map.singleton (PM.trTask tr) (tr NE.:| [])

timereportsByTask
    :: MonadPlanMillQuery m
    => Day                  -- ^ beginning of current month
    -> PM.TaskId
    -> NE.NonEmpty PM.Timereport
    -> m TimereportsByTask
timereportsByTask midDay taskId reports = do
    task <- PMQ.task taskId
    project <- traverse PMQ.project (PM.taskProject task)
    -- TODO: use foldl package
    let (prev, curr) = partitionEithers $ map classifyReports $ toList reports
    let prev' = map PM.trAmount prev
    let curr' = map PM.trAmount curr
    pure $ TimereportsByTask
        { _tbtProjectName    = maybe "" PM.pName project
        , _tbtTaskName       = PM.taskName task
        , _tbtHoursPrevMonth = TimereportsStats
            { _tsHours = ndtConvert' $ sum prev'
            , _tsCount = length prev
            , _tsMedian = ndtConvert' $ median 0 prev'
            }
        , _tbtHoursCurrMonth = TimereportsStats
            { _tsHours = ndtConvert' $ sum curr'
            , _tsCount = length curr
            , _tsMedian = ndtConvert' $ median 0 curr'
            }
        }
  where
    classifyReports r
        | PM.trStart r < midDay = Left r
        | otherwise             = Right r

-- almost correct
median :: Ord a => a -> [a] -> a
median d [] = d
median _ x  = sort x !! n2
  where
    n2 = length x `div` 2
