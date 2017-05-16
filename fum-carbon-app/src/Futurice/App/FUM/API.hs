{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.FUM.API where

import Prelude ()
import Futurice.Prelude

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.HTML.Lucid        (HTML)

type ChecklistAPI = IndexPageEndpoint

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy

-------------------------------------------------------------------------------
-- Index
-------------------------------------------------------------------------------

type IndexPageEndpoint =
    SSOUser :>
    Get '[HTML] (HtmlPage "indexpage")

indexPageEndpoint :: Proxy IndexPageEndpoint
indexPageEndpoint = Proxy
