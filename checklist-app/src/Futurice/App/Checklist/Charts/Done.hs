{-# LANGUAGE DataKinds #-}
module Futurice.App.Checklist.Charts.Done (doneChart) where

import Prelude ()
import Futurice.Prelude
import Control.Lens     (ifoldMapOf, to, (.=))
import Data.Time        (diffDays)
import Servant.Chart    (Chart (..))

import qualified Graphics.Rendering.Chart.Easy as C

import Futurice.App.Checklist.Types

doneChart
    :: World       -- ^ the world
    -> Day         -- ^ today
    -> AuthUser
    -> Chart "done"
doneChart world today _ = Chart . C.toRenderable $ do
    C.layout_title .= "tasks done per employee"
    C.layout_x_axis . C.laxis_title .= "starting day offset from today"
    C.layout_y_axis . C.laxis_title .= "done %"

    C.plot $ pure $ C.PlotPoints
        { C._plot_points_title = "All"
        , C._plot_points_style = C.def
            & C.point_color        .~ C.opaque C.black
            & C.point_border_color .~ C.opaque C.black
            & C.point_border_width .~ 1
            & C.point_radius       .~ 4
            & C.point_shape        .~ C.PointShapePlus
        , C._plot_points_values = mkAllPoint <$> rawPoints
        }

    for_ [minBound .. maxBound] $ \role ->
        C.plot $ pure $ C.PlotPoints
            { C._plot_points_title = taskRoleToText role ^. unpacked
            , C._plot_points_style = C.def
                & C.point_color        .~ C.opaque (colors ^. ix role)
                & C.point_border_color .~ C.opaque (colors ^. ix role)
                & C.point_border_width .~ 1
                & C.point_radius       .~ 4
                & C.point_shape        .~ C.PointShapePlus
            , C._plot_points_values = mkViewerPoint role <$> rawPoints
            }
 where
    colors = PerTaskRole C.blue C.green C.red

    rawPoints :: [(Integer, TodoCounter)]
    rawPoints =
        (world ^.. worldEmployees . folded . to mkPoint)
        ++ (world ^.. worldArchive . folded . to mkArchivedPoint)

    mkAllPoint :: (Integer, TodoCounter) -> (Double, C.Percent)
    mkAllPoint (d, TodoCounter (Counter i j) _) =
        ( fromInteger d
        , C.Percent $ 100 * fromIntegral i / fromIntegral j
        )

    mkViewerPoint :: TaskRole -> (Integer, TodoCounter) -> (Double, C.Percent)
    mkViewerPoint r (d, TodoCounter _ perRole) = case perRole ^. ix r of
        Counter i j ->
            ( fromInteger d
            , C.Percent $ 100 * fromIntegral i / fromIntegral j
            )

    mkPoint :: Employee -> (Integer, TodoCounter)
    mkPoint e = (diffDays startingDay today, counter)
      where
        eid = e ^. identifier
        startingDay = e ^. employeeStartingDay
        counter = ifoldMapOf
            (worldTaskItems . ix eid . ifolded)
            (toTodoCounter world)
            world

    mkArchivedPoint :: (Employee, TodoCounter) -> (Integer, TodoCounter)
    mkArchivedPoint (e, counter) = (diffDays startingDay today, counter)
      where
        startingDay = e ^. employeeStartingDay
