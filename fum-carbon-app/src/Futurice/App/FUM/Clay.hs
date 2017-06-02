{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.FUM.Clay where

import Prelude ()
import Futurice.Prelude          hiding ((&), (**))
import Clay
import Futurice.Lucid.Foundation
       (PageParams, defPageParams, embedJS, pageCss, pageJQuery,
       pageJs)

import qualified Control.Lens as L

pageParams :: PageParams
pageParams = defPageParams
    L.& pageCss    .~ [ css ]
    L.& pageJs     .~ [ $(embedJS "futu.js"), $(embedJS "lomake.js"), $(embedJS "fum-carbon.js") ]
    L.& pageJQuery .~ True

css :: Css
css = do
    header ? do
        marginTop $ em 1
        marginBottom $ em 1

    label # ".error" ? do
        color red
    "input[type=text]" # ".error" ? do
        borderColor red
    "input[type=date]" # ".error" ? do
        borderColor red
    "select" # ".error" ? do
        borderColor red

    label # ".pending" ? do
        color orange
    "input[type=text]" # ".pending" ? do
        borderColor orange
    "input[type=date]" # ".pending" ? do
        borderColor orange
    "select" # ".pending" ? do
        borderColor orange
