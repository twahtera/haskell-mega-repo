{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Futurice.App.Reports.MissingHoursChart (missingHoursChart) where

import Prelude ()
import Futurice.Prelude
import Control.Lens                (contains, (.=))
import Control.Monad.State.Strict  (StateT, evalStateT, state)
import Data.Fixed                  (Centi)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Futurice.Integrations
import Futurice.Time
import Numeric.Interval.NonEmpty   ((...))
import Servant.Chart               (Chart (..))

import Futurice.App.Reports.MissingHours

import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.Chart.Easy as C
import qualified PlanMill                      as PM

missingHoursChart
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => Set (PM.EnumValue PM.User "contractType")
    -> m (Chart "missing-hours")
missingHoursChart contractTypes = Chart . C.toRenderable <$> c
  where
    c :: m (C.EC (C.Layout Day Double) ())
    c = do
        -- interval: from beginning of the year
        today <- currentDay
        let (currYear, currWeek, _) = toWeekDate today
        let weekA = max 1 (currWeek - 13)
            weekB = max 1 (currWeek - 1)
        let interval =
                fromWeekDate currYear weekA 1 ...
                fromWeekDate currYear weekB 7

        -- people: do not include only some contracts
        fpm0 <- snd <$$> fumPlanmillMap
        let fpm1 :: [PM.User]
            fpm1 = filter (\e -> contractTypes ^. contains (PM.uContractType e)) (toList fpm0)

        -- timereports
        trs' <- for fpm1 $ \u -> (,)
            <$> planmillEmployee (u ^. PM.identifier)
            <*> missingHoursForUser interval u
        let trs = arrangeReports trs'

        pure $ do
            C.layout_title .= "Missing hours per employee per week: " ++ show interval
            flip evalStateT lineStyles $ ifor_ trs $ \tribe (count, hours) -> do
                let scale :: NDT 'Hours Centi -> Double
                    scale x = realToFrac (getNDT x) / fromIntegral (getSum count)
                lineStyle <- nextLineStyle
                lift $ C.plot $ line' lineStyle (tribe ^. unpacked) $ singleton $ do
                    week <- [weekA .. weekB]
                    let day = fromWeekDate currYear week 1
                    pure (day, maybe 0 scale $ hours ^? ix (currYear, week))

type Tribe = Text
type Count = Int
type YearWeek = (Integer, Int)

arrangeReports
    :: [(Employee, Vector MissingHour)]
    -> Map Tribe (Sum Count, Map YearWeek (NDT 'Hours Centi))
arrangeReports = Map.fromListWith f . map process
  where
    -- I wish we had better Monoid (Map k v)
    f (x, m) (x', m') = (x <> x', Map.unionWith (+) m m')

    process (e, hours) =
        ( employeeTeam e
        , (Sum 1, Map.fromListWith (+) $ map process2 $ toList hours)
        )
    process2 mh = ((y, w), mh ^. missingHourCapacity)
      where
        (y, w, _) = toWeekDate (mh ^. missingHourDay)


singleton :: a -> [a]
singleton x = [x]

-- Move to Futurice.Time
getNDT :: NDT tu x -> x
getNDT (NDT x) = x

line' :: C.LineStyle -> String -> [[(x,y)]]  -> C.EC l (C.PlotLines x y)
line' lineStyle title values = C.liftEC $ do
    C.plot_lines_title .= title
    C.plot_lines_values .= values
    C.plot_lines_style .= lineStyle

lineStyles :: [C.LineStyle]
lineStyles = cycle
    [ C.def & C.line_color .~ C.opaque c & C.line_dashes .~ d
    | d <- ds, c <- cs
    ]
  where
    cs = [ C.red, C.blue, C.green, C.magenta, C.orange ]
    ds = [[], [1,1], [5,5]]

nextLineStyle ::  Monad m => StateT [C.LineStyle] m C.LineStyle
nextLineStyle = state $ \s -> case s of
    (x:xs) -> (x, xs)
    []     -> (C.def, [])
