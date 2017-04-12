{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      :  Servant.Chart
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- An 'SVG' empty data type with 'MimeRender' instances for @Chart@.
--
-- >>> type ChartGet a = Get '[SVG] a
module Servant.Chart (SVG) where

import Data.Typeable    (Typeable)
import Servant.API      (Accept (..), MimeRender (..))
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Rendering.Chart.Backend          (vectorAlignmentFns)
import Graphics.Rendering.Chart.Backend.Diagrams (DEnv, defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Renderable       (ToRenderable (..))

import qualified Diagrams             as D
import qualified Diagrams.Backend.SVG as DSVG
import qualified Graphics.Svg.Core    as S
import qualified Network.HTTP.Media   as M

-------------------------------------------------------------------------------
-- Servant
-------------------------------------------------------------------------------

data SVG deriving Typeable

-- | @image/svg+xml@
instance Accept SVG where
    contentType _ = "image" M.// "svg+xml"

instance ToRenderable a => MimeRender SVG a where
    mimeRender _
        = S.renderBS
        . D.renderDia DSVG.SVG opts
        . fst
        . runBackendR denv
        . toRenderable
      where
        opts = DSVG.SVGOptions
            { DSVG._size            = D.dims2D w h
            , DSVG._svgDefinitions  = Nothing
            , DSVG._idPrefix        = mempty
            , DSVG._svgAttributes   = []
            , DSVG._generateDoctype = True
            }
        -- TODO: make configurable
        w = 1000
        h = 700

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------


denv :: DEnv Double
denv = unsafePerformIO $ defaultEnv vectorAlignmentFns 1000 700
{-# NOINLINE denv #-}
