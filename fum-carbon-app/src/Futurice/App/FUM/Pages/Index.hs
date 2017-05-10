{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.Index (indexPage) where

import Prelude ()
import Futurice.Prelude

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

indexPage
    :: World       -- ^ the world
    -> HtmlPage "indexpage"
indexPage _world = fumPage_ "FUM" () $ 
    pure ()
