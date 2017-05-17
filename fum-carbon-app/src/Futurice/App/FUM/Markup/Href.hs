{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Markup.Href where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.Utils.Links (Link, safeLink)
import Web.HttpApiData           (toUrlPiece)

import Futurice.App.FUM.API

import qualified Personio

indexPageHref_ :: Attribute
indexPageHref_ = href_ $ linkToText $ safeLink fumCarbonApi
    indexPageEndpoint

createEmployeeHref_ :: Personio.EmployeeId -> Attribute
createEmployeeHref_ eid = href_ $ linkToText $ safeLink fumCarbonApi
    createEmployeePageEndpoint
    eid

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l
