{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Clay where

import Lucid.Foundation.Futurice (pageCss, PageParams, defPageParams)
import Prelude ()
import Futurice.Prelude hiding ((&), (**))
import Clay

pageParams :: PageParams
pageParams =
    (pageCss .~ [css]) defPageParams

css :: Css
css = do
    a ? do
        color (darken 0.2 green)
        hover & color green
    table ? tbody ? tr ? do
        nthChild "even" & do
            ".eta-today" & backgroundColor yellow
            ".eta-future" & backgroundColor green
            ".eta-past" & backgroundColor (lighten 0.95 red)
        nthChild "odd" & do
            ".eta-today" & backgroundColor yellow
            ".eta-future" & backgroundColor green
            ".eta-past" & backgroundColor (lighten 0.97 red)
