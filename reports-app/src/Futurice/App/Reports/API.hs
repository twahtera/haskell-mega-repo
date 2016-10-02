{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports.API where

import Futurice.App.Reports.Types
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Servant
import Servant.CSV.Cassava (CSV', DefaultOpts)

type ReportTypes = '[HTML, (CSV', DefaultOpts), JSON]

type ReportsAPI = Get '[HTML] (HtmlPage "index")
    :<|> "issues" :> Get ReportTypes IssueReport
    :<|> "fum-github" :> Get ReportTypes FumGitHubReport

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy
