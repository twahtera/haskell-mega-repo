{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Reports.UtzChart (utzChart) where

import Prelude ()
import Futurice.Prelude
import Control.Lens                (Getter, to, toListOf, (.=))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Integrations
import Futurice.Monoid             (Average (..))
import Futurice.Time

import Futurice.App.Reports.Chart

import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.Chart.Easy as C
import qualified PlanMill                      as PM
import qualified PlanMill.Queries              as PMQ

utzChart
    :: forall m. ( MonadTime m, MonadPlanMillQuery m)
    => m (Chart "utz")
utzChart = Chart . C.toRenderable <$> c
  where
    c :: m (C.EC (C.Layout Double Double) ())
    c = do
        today <- currentDay
        uids <- view PM.identifier <$$> PMQ.users
        -- TODO: limit the parallelism
        trs <- toListOf (folded . folded) <$>
            traverse (PMQ.timereports (interval today)) uids
        let utzs = timereportUtzPerWeek trs
        pure $ do
            C.layout_title .= "UTZ per week"
            C.plot $ C.line "2017"
                [[(fromIntegral w, fromMaybe 0 $ utzs ^? ix (2017, w)) | w <- [1..53]]]
            C.plot $ C.line "2016"
                [[(fromIntegral w, fromMaybe 0 $ utzs ^? ix (2016, w)) | w <- [1..53]]]
            C.plot $ C.line "2015"
                [[(fromIntegral w, fromMaybe 0 $ utzs ^? ix (2015, w)) | w <- [1..53]]]

    interval today = $(mkDay "2015-01-01") PM.... today

timereportUtzPerWeek :: [PM.Timereport] -> Map (Integer, Int) Double
timereportUtzPerWeek = fmap getAverage . Map.fromListWith (<>) . fmap mk
  where
    mk :: PM.Timereport -> ((Integer, Int), Average Double)
    mk tr = ((y, w), tr ^. reportUtilizationAvg)
      where
        (y, w, _) = toWeekDate (PM.trStart tr)

-------------------------------------------------------------------------------
-- Copied from hours-api
-------------------------------------------------------------------------------

billableStatus :: Maybe PM.ProjectId -> Int -> EntryType
billableStatus Nothing 3 = EntryTypeOther
billableStatus _ 3       = EntryTypeNotBillable
billableStatus _ _       = EntryTypeBillable

reportUtilizationAvg :: Getter PM.Timereport (Average Double)
reportUtilizationAvg = to $ \tr ->
    let NDT hours = ndtConvert' (PM.trAmount tr) :: NDT 'Hours Double
    in case billableStatus (PM.trProject tr) (PM.trBillableStatus tr) of
        EntryTypeBillable    -> Average hours 100
        EntryTypeNotBillable -> Average hours 0
        EntryTypeOther       -> mempty

data EntryType
    = EntryTypeBillable
    | EntryTypeNotBillable
    | EntryTypeOther
