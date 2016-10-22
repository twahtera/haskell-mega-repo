{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports.API where

import Prelude ()
import Futurice.Prelude
import Futurice.App.Reports.Balances     (BalanceReport)
import Futurice.App.Reports.MissingHours (MissingHoursReport)
import Futurice.App.Reports.Types
import Futurice.Lucid.Foundation
import Futurice.Servant
import GHC.TypeLits (Symbol)
import Servant

type ReportTypes = '[HTML, CSV, JSON]

data R (path :: Symbol) (report :: *)

type Reports =
    '[ R "issues"       IssueReport
    , R "fum-github"    FumGitHubReport
    , R "missing-hours" MissingHoursReport
    , R "balances"      BalanceReport
    ]

-- | This and 'RReport' type families are needed to make 'FoldReportsAPI' reduce
-- to the ':<|>' in cons case.
type family RPath r where
    RPath (R path report) = path

type family RReport r where
    RReport (R path report) = report

type family FoldReportsAPI rs :: * where
    FoldReportsAPI '[]       = Get '[HTML] (HtmlPage "index")
    FoldReportsAPI (r ': rs) =
        RPath r :> Get ReportTypes (RReport r) :<|> FoldReportsAPI rs

type ReportsAPI = FoldReportsAPI Reports

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy
