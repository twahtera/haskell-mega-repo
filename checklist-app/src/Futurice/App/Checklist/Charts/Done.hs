{-# LANGUAGE DataKinds #-}
module Futurice.App.Checklist.Charts.Done (doneChart) where

import Prelude ()
import Futurice.Prelude
import Control.Lens ((.=), ifoldMapOf)
import Servant.Chart    (Chart (..))
import Data.Time                 (diffDays)

import qualified Graphics.Rendering.Chart.Easy as C

import Futurice.App.Checklist.Markup (TodoCounter (..), toTodoCounter)
import Futurice.App.Checklist.Types

doneChart
    :: World       -- ^ the world
    -> Day         -- ^ today
    -> AuthUser
    -> Chart "done"
doneChart world today (_fu, viewerRole) = Chart . C.toRenderable $ do
    C.layout_title .= "tasks done per employee"
    C.layout_y_axis . C.laxis_title .= "day offset"
    C.layout_y_axis . C.laxis_title .= "done"

    C.plot $ pure $ C.PlotPoints
        { C._plot_points_title = "All"
        , C._plot_points_style = C.def
            & C.point_color        .~ C.opaque C.red
            & C.point_border_color .~ C.opaque C.red
            & C.point_border_width .~ 1
            & C.point_radius       .~ 4
            & C.point_shape        .~ C.PointShapePlus
        , C._plot_points_values = mkAllPoint <$> rawPoints
        }

    C.plot $ pure $ C.PlotPoints
        { C._plot_points_title = taskRoleToText viewerRole ^. unpacked
        , C._plot_points_style = C.def
            & C.point_color        .~ C.opaque C.blue
            & C.point_border_color .~ C.opaque C.blue
            & C.point_border_width .~ 1
            & C.point_radius       .~ 4
            & C.point_shape        .~ C.PointShapePlus
        , C._plot_points_values = mkViewerPoint <$> rawPoints
        }
 where
    rawPoints :: [(Integer, TodoCounter)]
    rawPoints = mkPoint <$> world ^.. worldEmployees . folded

    mkAllPoint :: (Integer, TodoCounter) -> (Double, C.Percent)
    mkAllPoint (d, TodoCounter _ _ i j) =
        ( fromInteger d
        , C.Percent $ 100 * fromIntegral i / fromIntegral j
        )

    mkViewerPoint :: (Integer, TodoCounter) -> (Double, C.Percent)
    mkViewerPoint (d, TodoCounter i j _ _) =
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
            (toTodoCounter world viewerRole)
            world
