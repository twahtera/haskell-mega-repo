{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt.Types (
    AccessReport,
    UsersReport,
    Ctx (..),
    ) where

import Futurice.Prelude

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.Report            (Report, ReportGenerated)

-------------------------------------------------------------------------------
-- AccessReport
-------------------------------------------------------------------------------

-- | who, when, what
type AccessReport = Report
    "Access report"
    ReportGenerated
    (Vector :$ NP I '[Text, UTCTime, Text])

-------------------------------------------------------------------------------
-- UsersReport
-------------------------------------------------------------------------------

-- | who, by whom, when
type UsersReport = Report
    "Users report"
    ReportGenerated
    (Vector :$ NP I '[Text, Text, UTCTime])

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxPostgresPool :: !(Pool Connection)
    }
