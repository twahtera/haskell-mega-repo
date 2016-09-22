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
    header ? do
        marginTop $ em 1
        marginBottom $ em 1
    table ? tbody ? tr ? do
        nthChild "even" & do
            ".eta-far-past"    & backgroundColor (lighten 0.92 blue)
            ".eta-past"        & backgroundColor (lighten 0.92 green)
            ".eta-today"       & backgroundColor (lighten 0.92 yellow)
            ".eta-near-future" & backgroundColor (lighten 0.90 orange)
            ".eta-future"      & backgroundColor (lighten 0.92 red)
            ".eta-far-future"  & backgroundColor (lighten 0.92 violet)
        nthChild "odd" & do
            ".eta-far-past"    & backgroundColor (lighten 0.95 blue)
            ".eta-past"        & backgroundColor (lighten 0.95 green)
            ".eta-today"       & backgroundColor (lighten 0.95 yellow)
            ".eta-near-future" & backgroundColor (lighten 0.92 orange)
            ".eta-future"      & backgroundColor (lighten 0.95 red)
            ".eta-far-future"  & backgroundColor (lighten 0.95 violet)
