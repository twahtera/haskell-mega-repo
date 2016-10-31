{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
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

data TimereportsByTask = TimereportsByTask
    { _tbtProjectName :: !Text
    , _tbtTaskName    :: !Text
    , _tbtHoursPrevMonth :: !(NDT 'Hours Centi)
    , _tbtHoursCurrMonth :: !(NDT 'Hours Centi)
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
    let (prev, curr) = partitionEithers $ map classifyReports $ toList reports
    pure $ TimereportsByTask
        { _tbtProjectName    = maybe "" PM.pName project
        , _tbtTaskName       = PM.taskName task
        , _tbtHoursPrevMonth = ndtConvert' $ sum $ map PM.trAmount prev
        , _tbtHoursCurrMonth = ndtConvert' $ sum $ map PM.trAmount curr
        }
  where
    classifyReports r
        | PM.trStart r < midDay = Left r
        | otherwise             = Right r
