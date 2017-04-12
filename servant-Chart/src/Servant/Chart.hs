{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      :  Servant.Chart
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- An 'SVG' empty data type with 'MimeRender' instances for @Chart@.
--
-- >>> type ChartGet a = Get '[SVG] a
module Servant.Chart (SVG) where

import Data.FileEmbed   (embedFile, makeRelativeToProject)
import Data.Typeable    (Typeable)
import Servant.API      (Accept (..), MimeRender (..))

import Graphics.Rendering.Chart.Backend.Diagrams
       (DEnv, FontSelector, createEnv, runBackendR)
import Graphics.Rendering.Chart.Renderable       (ToRenderable (..))

import qualified Diagrams                         as D
import qualified Diagrams.Backend.SVG             as DSVG
import qualified Graphics.Rendering.Chart.Backend as B
import qualified Graphics.Svg.Core                as S
import qualified Graphics.SVGFonts.ReadFont       as F
import qualified Network.HTTP.Media               as M

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
denv = createEnv B.vectorAlignmentFns 1000 700 loadSansSerifFonts
{-# NOINLINE denv #-}

loadSansSerifFonts :: FontSelector Double
loadSansSerifFonts = selectFont
  where
    sansR = snd $ F.loadFont' "SourceSansPro_R" $(makeRelativeToProject "fonts/SourceSansPro_R.svg" >>= embedFile)
    sansRB = snd $ F.loadFont' "SourceSansPro_RB" $(makeRelativeToProject "fonts/SourceSansPro_RB.svg" >>= embedFile)
    sansRBI = snd $ F.loadFont' "SourceSansPro_RBI" $(makeRelativeToProject "fonts/SourceSansPro_RBI.svg" >>= embedFile)
    sansRI = snd $ F.loadFont' "SourceSansPro_RI" $(makeRelativeToProject "fonts/SourceSansPro_RI.svg" >>= embedFile)

    selectFont :: B.FontStyle -> F.PreparedFont Double
    selectFont fs = case (B._font_name fs, B._font_slant fs, B._font_weight fs) of
        (_, B.FontSlantNormal , B.FontWeightNormal) -> alterFontFamily "sans-serif" sansR
        (_, B.FontSlantNormal , B.FontWeightBold  ) -> alterFontFamily "sans-serif" sansRB
        (_, B.FontSlantItalic , B.FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
        (_, B.FontSlantOblique, B.FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
        (_, B.FontSlantItalic , B.FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI
        (_, B.FontSlantOblique, B.FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI
{-# NOINLINE loadSansSerifFonts #-}

alterFontFamily :: String -> F.PreparedFont n -> F.PreparedFont n
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)
