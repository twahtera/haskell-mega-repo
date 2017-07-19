{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Smileys.Charts where

import Control.Lens     (to, (.=), (^@..))
import Data.Pool        (withResource)
import Futurice.Prelude
import Prelude ()
import Servant.Chart    (Chart (..))

import Futurice.App.Smileys.Ctx
import Futurice.App.Smileys.Types

import qualified Data.Map                      as Map
import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Graphics.Rendering.Chart.Easy as C

-------------------------------------------------------------------------------
-- Absolute
-------------------------------------------------------------------------------

absoluteChartHandler :: MonadBaseControl IO m => Ctx -> m (Chart "absolute")
absoluteChartHandler = chartHandler chart
  where
    chart values = Chart . C.toRenderable $ do
        C.layout_title .= "absolute smileys per day"
        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "smileys count"

        C.plot $ pure $ strip ":(" C.red    $ values ^@.. ifolded . to firstStrip
        C.plot $ pure $ strip ":|" C.yellow $ values ^@.. ifolded . to secondStrip
        C.plot $ pure $ strip ":)" C.blue   $ values ^@.. ifolded . to thirdStrip

    strip
        :: String -> C.Colour Double -> [(Day, (Int, Int))]
        -> C.PlotFillBetween Day Int
    strip title colour xs = C.def
        & C.plot_fillbetween_title  .~ title
        & C.plot_fillbetween_style  .~ (C.def & C.fill_color .~ C.opaque colour)
        & C.plot_fillbetween_values .~ xs

-------------------------------------------------------------------------------
-- Relative
-------------------------------------------------------------------------------

relativeChartHandler :: MonadBaseControl IO m => Ctx -> m (Chart "relative")
relativeChartHandler = chartHandler chart
  where
    chart values = Chart . C.toRenderable $ do
        C.layout_title .= "relative smileys per day"
        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "smileys %"

        C.plot $ pure $ strip ":(" C.red    $ values ^@.. ifolded . to firstStripR
        C.plot $ pure $ strip ":|" C.yellow $ values ^@.. ifolded . to secondStripR
        C.plot $ pure $ strip ":)" C.blue   $ values ^@.. ifolded . to thirdStripR

    strip
        :: String -> C.Colour Double -> [(Day, (Double, Double))]
        -> C.PlotFillBetween Day Double
    strip title colour xs = C.def
        & C.plot_fillbetween_title  .~ title
        & C.plot_fillbetween_style  .~ (C.def & C.fill_color .~ C.opaque colour)
        & C.plot_fillbetween_values .~ xs

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

chartHandler
    :: MonadBaseControl IO m
    => (Map Day SmileyAcc -> Chart a)
    -> Ctx -> m (Chart a)
chartHandler chart ctx = do
    input <- withResource (ctxPostgresPool ctx) $ \conn ->
        liftBase $ Postgres.query_ conn "SELECT day, smiley FROM smileys.trail"
    pure $ chart $
        Map.fromListWith (<>) $ fmap (second smileyAcc) input

-------------------------------------------------------------------------------
-- Smiley accumulator (per day)
-------------------------------------------------------------------------------

data SmileyAcc = SmileyAcc !Int !Int !Int

instance Semigroup SmileyAcc where
    SmileyAcc a b c <> SmileyAcc x y z = SmileyAcc (a + x) (b + y) (c + z)

instance Monoid SmileyAcc where
    mempty = SmileyAcc 0 0 0
    mappend = (<>)

smileyAcc :: SmileyValue -> SmileyAcc
smileyAcc (SmileyValue 0) = SmileyAcc 1 0 0
smileyAcc (SmileyValue 1) = SmileyAcc 0 1 0
smileyAcc (SmileyValue 2) = SmileyAcc 0 0 1
smileyAcc (SmileyValue _) = SmileyAcc 0 0 0

firstStrip :: SmileyAcc -> (Int, Int)
firstStrip (SmileyAcc x _ _) = (0, x)

secondStrip :: SmileyAcc -> (Int, Int)
secondStrip (SmileyAcc x y _) = (x, x + y)

thirdStrip :: SmileyAcc -> (Int, Int)
thirdStrip (SmileyAcc x y z) = (x + y, x + y + z)

firstStripR :: SmileyAcc -> (Double, Double)
firstStripR (SmileyAcc x y z)
    | total <= 0 = (0, 0)
    | otherwise = (0, 100 * fromIntegral x / total')
  where
    total = x + y + z
    total' = fromIntegral total

secondStripR :: SmileyAcc -> (Double, Double)
secondStripR (SmileyAcc x y z)
    | total <= 0 = (0, 100)
    | otherwise =
      ( 100 * fromIntegral x / total'
      , 100 * fromIntegral (x + y) / total'
      )
  where
    total  = x + y + z
    total' = fromIntegral total

thirdStripR :: SmileyAcc -> (Double, Double)
thirdStripR (SmileyAcc x y z)
    | total <= 0 = (100, 100)
    | otherwise =
        ( 100 * fromIntegral (x + y) / total'
        , 100 * fromIntegral (x + y + z) / total'
        )
  where
    total = x + y + z
    total' = fromIntegral total
