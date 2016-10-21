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
import Servant

type ReportTypes = '[HTML, CSV, JSON]

type ReportsAPI = Get '[HTML] (HtmlPage "index")
    :<|> "issues" :> Get ReportTypes IssueReport
    :<|> "fum-github" :> Get ReportTypes FumGitHubReport
    :<|> "missing-hours" :> Get ReportTypes MissingHoursReport
    :<|> "balances" :> Get ReportTypes BalanceReport

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy
