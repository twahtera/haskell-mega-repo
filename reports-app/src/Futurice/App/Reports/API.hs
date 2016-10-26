{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports.API where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation
import Futurice.Report.Columns   (Report)
import Futurice.Servant
import GHC.TypeLits              (KnownSymbol, Symbol)
import Servant

import Futurice.App.Reports.Balances     (BalanceReport)
import Futurice.App.Reports.FumGithub    (FumGitHubReport)
import Futurice.App.Reports.GithubIssues (IssueReport)
import Futurice.App.Reports.MissingHours (MissingHoursReport)
import Futurice.App.Reports.PowerUser    (PowerUserReport)
import Futurice.App.Reports.PowerAbsences    (PowerAbsenceReport)

type ReportTypes = '[HTML, CSV, JSON]

data R (path :: Symbol) (report :: *)

type Reports =
    '[ R "issues"        IssueReport
    , R "fum-github"     FumGitHubReport
    , R "missing-hours"  MissingHoursReport
    , R "balances"       BalanceReport
    , R "power-users"    PowerUserReport
    , R "power-absences" PowerAbsenceReport
    ]

-- | This, 'RReport' and 'RName', type families are needed to make 'FoldReportsAPI' reduce
-- to the ':<|>' in cons case.
type family RPath r where
    RPath (R path report) = path

type family RReport r where
    RReport (R path report) = report

type family RName r where
    RName (R path (Report name params a)) = name

class (KnownSymbol (RPath r), KnownSymbol (RName r), NFData (RReport r)) => RClass r
instance (KnownSymbol path, KnownSymbol name, NFData params, NFData a)
    => RClass (R path (Report name params a))

type family FoldReportsAPI rs :: * where
    FoldReportsAPI '[]       = Get '[HTML] (HtmlPage "index")
    FoldReportsAPI (r ': rs) =
        RPath r :> Get ReportTypes (RReport r) :<|>
        RPath r :> "json" :> Get '[JSON] (RReport r) :<|>
        RPath r :> "csv" :> Get '[CSV] (RReport r) :<|>
        FoldReportsAPI rs

type ReportsAPI = FoldReportsAPI Reports

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy
