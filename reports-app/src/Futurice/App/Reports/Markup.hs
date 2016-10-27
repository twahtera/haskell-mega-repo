{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.Markup (indexPage) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Generics.SOP (hpure, hsequenceK, hcmap)
import GHC.TypeLits (symbolVal, KnownSymbol)

import Futurice.App.Reports.API

links :: NP Proxy Reports
links = hpure Proxy

makeLink :: forall r m. (RClass r, Monad m) => Proxy r -> K (HtmlT m ()) r
makeLink _ = K $ li_ $ do
    a_ [ href_ $ "/" <> textVal ppath ] $ toHtml $ textVal pname
    " ["
    -- TODO: we'd like to use ".json", but we need type-level sybmol concatenation
    -- https://ghc.haskell.org/trac/ghc/ticket/12162
    a_ [ href_ $ "/" <> textVal ppath <> "/json" ] $ "JSON"
    "] ["
    a_ [ href_ $ "/" <> textVal ppath <> "/csv" ] $ "CSV"
    "]"
  where
    ppath = Proxy :: Proxy (RPath r)
    pname = Proxy :: Proxy (RName r)

-- | TODO: Move to @futurice-prelude@
textVal :: KnownSymbol a => Proxy a -> Text
textVal p = symbolVal p ^. packed

indexPage :: HtmlPage "index"
indexPage = page_ "Reports" $ do
    row_ $ large_ 12 $ h1_ "Reports"
    row_ $ large_ 12 $ div_ [class_ "callout primary"] $ ul_ $ void $ hsequenceK $
        hcmap (Proxy :: Proxy RClass) makeLink links
