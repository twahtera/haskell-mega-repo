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
            ".eta-today"  & backgroundColor (lighten 0.92 yellow)
            ".eta-past"   & backgroundColor (lighten 0.92 green)
            ".eta-future" & backgroundColor (lighten 0.92 red)
        nthChild "odd" & do
            ".eta-today"  & backgroundColor (lighten 0.95 yellow)
            ".eta-past"   & backgroundColor (lighten 0.95 green)
            ".eta-future" & backgroundColor (lighten 0.95 red)
