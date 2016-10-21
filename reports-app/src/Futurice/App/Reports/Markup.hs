{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.Markup (indexPage) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

indexPage :: HtmlPage "index"
indexPage = page_ "Reports" $ do
    row_ $ large_ 12 $ h1_ "Reports"
    row_ $ large_ 12 $ div_ [class_ "callout"] $ ul_ $ do
        li_ $ a_ [href_ "/issues" ]        $ "GitHub issues"
        li_ $ a_ [href_ "/fum-github" ]    $ "Users in FUM and GitHub"
        li_ $ a_ [href_ "/missing-hours" ] $ "Missing hour markings"
        li_ $ a_ [href_ "/balances" ]      $ "Hour marking flex saldos"
