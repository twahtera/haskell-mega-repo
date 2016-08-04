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

import qualified Servant.HTML.Lucid as Lucid

import Futurice.App.Reports.Types

type ReportTypes = '[Lucid.HTML, (CSV', DefaultEncodeOpts), JSON]

type ReportsAPI = Get '[Lucid.HTML] IndexPage
    :<|> "issues" :> Get ReportTypes IssueReport
    :<|> "fum-github" :> Get ReportTypes FumGitHubReport

type ReportsAPI' = FuturiceAPI ReportsAPI ('FutuAccent 'AF2 'AC3)

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy

reportsApi' :: Proxy ReportsAPI'
reportsApi' = Proxy
