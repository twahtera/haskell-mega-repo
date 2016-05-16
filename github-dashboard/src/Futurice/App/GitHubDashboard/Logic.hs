{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.GitHubDashboard.Logic (
    PullRequests,
    pullrequests,
    Issues,
    issues,
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Time           (getCurrentTime)
import Network.HTTP.Client (Manager)

import qualified Data.Map.Strict            as Map
import qualified Database.PostgreSQL.Simple as Postgres
import qualified GitHub                     as GH

import Futurice.App.GitHubDashboard.Types

import Debug.Trace

-- | TODO: MonadPostgres, MonadGithub
pullrequests
    :: Manager
    -> Postgres.Connection
    -> GH.Auth
    -> IO PullRequests
pullrequests mgr conn auth = do
    repos <- Postgres.query_ conn "SELECT owner, repo FROM githubdashboard.repos;"
    PullRequests . Map.fromList . filter (not . null . snd) <$> traverse fetch repos
  where
    fetch k@(owner, repo) = (k,) . fmap t . e <$>
       GH.executeRequestWithMgr mgr auth (GH.pullRequestsForR owner repo opts Nothing)
    opts = GH.defaultPullRequestOptions

    t pr = PullRequestInfo
        { _prNumber  = GH.simplePullRequestNumber pr
        , _prTitle   = GH.simplePullRequestTitle pr
        , _prCreated = GH.simplePullRequestCreatedAt pr
        , _prUrl     = GH.getUrl $ GH.simplePullRequestHtmlUrl pr
        }

    e (Right x) = x
    e (Left err)  = traceShow err mempty

issues
    :: Manager
    -> Postgres.Connection
    -> GH.Auth
    -> IO Issues
issues mgr conn auth = do
    now <- getCurrentTime
    repos <- Postgres.query_ conn "SELECT owner, repo FROM githubdashboard.repos;"
    Issues now . Map.fromList . filter (not . null . snd) <$> traverse fetch repos
  where
    fetch k@(owner, repo) = (k,) . fmap t . e <$>
       GH.executeRequestWithMgr mgr auth (GH.issuesForRepoR owner repo opts Nothing)
    opts = [GH.Open]

    t pr = IssueInfo
        { _issueNumber  = GH.issueNumber pr
        , _issueTitle   = GH.issueTitle pr
        , _issueCreated = GH.issueCreatedAt pr
        , _issueUrl     = maybe "" id $ GH.issueHtmlUrl pr
        }

    e (Right x) = x
    e (Left _)  = mempty
