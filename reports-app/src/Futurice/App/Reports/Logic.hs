{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Reports.Logic (
    IssueReport,
    issueReport,
    ) where

import Futurice.Prelude

import Data.Time           (getCurrentTime)
import Futurice.Report     (Per (..), Report (..), ReportGenerated (..))
import Network.HTTP.Client (Manager)

import qualified Data.Vector as V
import qualified GitHub      as GH

import Futurice.App.Reports.Types

issueReport
    :: Manager
    -> GH.Auth
    -> [GitHubRepo]
    -> IO IssueReport
issueReport mgr auth repos = do
    now <- getCurrentTime
    Report (ReportGenerated now) . V.fromList <$> traverse fetch repos
  where
    fetch :: GitHubRepo -> IO (Per GitHubRepo (Vector IssueInfo))
    fetch ghr@(GitHubRepo owner repo) = Per ghr . fmap t . e <$>
       GH.executeRequestWithMgr mgr auth (GH.issuesForRepoR owner repo opts Nothing)

    opts = [GH.Open]

    t pr = IssueInfo
        { _issueNumber  = GH.issueNumber pr
        , _issueTitle   = GH.issueTitle pr
        , _issueCreated = GH.issueCreatedAt pr
        , _issueUrl     = fromMaybe "" $ GH.issueHtmlUrl pr
        }

    e (Right x) = x
    e (Left _)  = mempty
