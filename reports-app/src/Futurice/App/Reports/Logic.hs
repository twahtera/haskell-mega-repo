{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Reports.Logic (
    IssueReport,
    issueReport,
    fumGithubReport,
    ) where

import Futurice.Prelude

import Control.Arrow ((&&&))
import Data.These          (These (..))
import Data.Maybe (mapMaybe)
import Data.Align          (Align(..))
import Data.Time           (getCurrentTime)
import Futurice.Report     (Per (..), Report (..), ReportGenerated (..))
import Network.HTTP.Client (Manager)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified FUM
import qualified GitHub              as GH

import Futurice.App.Reports.Config
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
       GH.executeRequestWithMgr mgr auth (GH.issuesForRepoR owner repo opts GH.FetchAll)

    opts = [GH.Open]

    t pr = IssueInfo
        { _issueNumber  = GH.issueNumber pr
        , _issueTitle   = GH.issueTitle pr
        , _issueCreated = GH.issueCreatedAt pr
        , _issueUrl     = maybe "" GH.getUrl $ GH.issueHtmlUrl pr
        }

    e (Right x) = x
    e (Left _)  = mempty

fumGithubReport
    :: Manager
    -> Config
    -> IO FumGitHubReport
fumGithubReport mgr cfg = do
    now <- getCurrentTime
    fs <- fumUsers
    gs <- githubUsers
    return $ Report (FumGithubReportParams now $ cfgFumPubUrl cfg) $ makeReport gs fs
  where
    fumUsers :: IO (Vector FUMUser)
    fumUsers = V.fromList . mapMaybe mk . V.toList <$>
        FUM.fetchList mgr (cfgFumAuth cfg) (cfgFumBaseUrl cfg) (cfgFumUserList cfg)
      where
        mk u = case u ^. FUM.userGithub ^. lazy of
            Nothing -> Nothing
            Just ghLogin -> Just $ FUMUser
                { fumUserName  = u ^. FUM.userFirst <> " " <> u ^. FUM.userLast
                , fumUserLogin = u ^. FUM.userName ^. FUM.getUserName
                , fumGhLogin   = ghLogin
                }

    githubUsers :: IO (Vector GitHubUser)
    githubUsers = do
        let team = cfgGhTeam cfg
        teams <- exec $ GH.teamsOfR (cfgGhOrg cfg) GH.FetchAll
        case V.find ((team ==) . GH.simpleTeamName) teams of
            Nothing ->
                traverse mk =<< exec (GH.membersOfR (cfgGhOrg cfg) GH.FetchAll)
            Just t ->
                traverse mk =<< exec (GH.listTeamMembersR (GH.simpleTeamId t) GH.TeamMemberRoleAll GH.FetchAll)

      where
        exec :: GH.Request k a -> IO a
        exec req = do
            print req
            r <- GH.executeRequestWithMgr mgr (cfgGhAuth cfg) req
            either throwM pure r

        mk u = do
            u' <- exec (GH.userInfoForR (GH.simpleUserLogin u))
            pure $ GitHubUser
                { ghUserName  = GH.userName u'
                , ghUserLogin = GH.untagName $ GH.userLogin u'
                }

    -- | Align on `
    makeReport
        :: Vector GitHubUser -> Vector FUMUser
        -> Vector (These GitHubUser FUMUser)
    makeReport gs fs =
        -- Create 'HashMap FUMLogin a' maps, and 'align' them
        let gs' = HM.fromList . map (ghUserLogin &&& id) . V.toList $ gs
            fs' = HM.fromList . map (fumGhLogin &&& id) . V.toList $ fs
            hm  = align gs' fs'
        in V.fromList . sort . HM.elems $ hm

{-
-- | TODO: Move to futurice-prelude
traverse2 :: (Applicative f, Traversable t, Traversable t') => (a -> f b) -> t (t' a) -> f (t (t' b))
traverse2 = traverse . traverse
-}
