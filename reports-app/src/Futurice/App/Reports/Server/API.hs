{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports.Server.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Servant
import Servant.CSV.Cassava (CSV', DefaultEncodeOpts)

import Futurice.App.Reports.Types

type ReportTypes = '[HTML, (CSV', DefaultEncodeOpts), JSON]

type ReportsAPI = Get '[HTML] IndexPage
    :<|> "issues" :> Get ReportTypes IssueReport
    :<|> "fum-github" :> Get ReportTypes FumGitHubReport

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy
