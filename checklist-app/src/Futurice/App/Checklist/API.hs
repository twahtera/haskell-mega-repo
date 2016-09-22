{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant   (SSOUser)
import Servant.API
import Servant.HTML.Lucid (HTML)

import Futurice.App.Checklist.Types.Page

type ChecklistAPI =
    SSOUser :> Get '[HTML] (Page "indexpage")

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy
