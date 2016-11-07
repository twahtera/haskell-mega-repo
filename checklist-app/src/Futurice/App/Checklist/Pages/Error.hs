{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Error (
    notFoundPage,
    forbiddedPage,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay

-- | 404
notFoundPage :: HtmlPage sym
notFoundPage = page_ "Not found" pageParams $ do
    row_ $ large_ 12 $ header_ $ h1_ $ "Not found"
    row_ $ large_ 12 $ p_ $
        "The requested document is no more. Even tried multi."

-- | 403
forbiddedPage :: HtmlPage sym
forbiddedPage = page_ "Forbidden" pageParams $ do
    row_ $ large_ 12 $ header_ $ h1_ $ "Forbidden"
    row_ $ large_ 12 $ p_ $
        "Ask IT-team to create you an account."
