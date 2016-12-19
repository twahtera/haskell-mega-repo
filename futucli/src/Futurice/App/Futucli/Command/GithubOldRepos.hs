{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Futucli.Command.GithubOldRepos (githubOldRepos) where

import Futurice.Prelude

import Algebra.Lattice ((\/))
import Data.Time       (diffUTCTime, getCurrentTime)
import System.IO       (stderr)

import GitHub
       (Auth (..), Commit, Name, Organization, Repo (..), RepoPublicity (..),
       SimpleUser (..), User, commitsForR, contributorToSimpleUser,
       contributorsR, fromOrganizationName, organizationReposR, untagName)

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as V
import qualified GitHub       as GH

import Futurice.App.Futucli.Cfg

-- | 365 days is good enough approximation.
daysToNominalDiffTime :: Integer -> NominalDiffTime
daysToNominalDiffTime days = fromInteger days * 24 * 60 * 60

distantPast :: UTCTime
distantPast = UTCTime (ModifiedJulianDay 0) 0

data R = R (Name Repo) [Name User]
    deriving (Eq, Ord, Show)

prettyR :: R -> IO ()
prettyR (R repo cs)
    | length cs > 10  = T.putStrLn $ untagName repo <> ": " <> T.intercalate ", " (map untagName $ take 10 cs) <> "..."
    | otherwise       = T.putStrLn $ untagName repo <> ": " <> T.intercalate ", " (map untagName cs)

fromName :: Manager -> Auth -> Name Organization -> Name Repo -> IO R
fromName mgr auth org repo = do
    T.hPutStrLn stderr $ "asking about " <> untagName repo
    cs <- executeRequestWithMgr mgr auth $ contributorsR (fromOrganizationName org) repo False $ GH.FetchAtLeast 10
    pure . R repo . map simpleUserLogin . mapMaybe contributorToSimpleUser . V.toList $ cs

repoOld :: UTCTime -> Integer -> (Repo, V.Vector Commit) -> Bool
repoOld now days
    = (> daysToNominalDiffTime days)
    . (now `diffUTCTime`)
    . fromMaybe distantPast
    . repoPushedAt
    . fst

repoSmall :: Int -> (Repo, V.Vector Commit) -> Bool
repoSmall commitsCount
    = (< commitsCount)
    . V.length
    . snd

getCommits :: Manager -> Auth -> Int -> Repo -> IO (Repo, V.Vector Commit)
getCommits mgr auth commitsCount repo = (repo,) <$>
    executeRequestWithMgr mgr auth (commitsForR (GH.simpleOwnerLogin . repoOwner $ repo) (repoName repo) (GH.FetchAtLeast $ fromIntegral commitsCount + 1))

githubOldRepos :: Cfg -> Integer -> Int -> IO ()
githubOldRepos cfg days commits = do
    let a = _cfgGhToken cfg
    let org = _cfgGhOrg cfg

    now <- getCurrentTime
    mgr <- newManager tlsManagerSettings

    repos' <- nub . V.toList <$> executeRequestWithMgr mgr a (organizationReposR org RepoPublicityAll GH.FetchAll)
    repos <- traverse (getCommits mgr a commits) repos'

    let privRepos = filter (repoPrivate . fst) repos
    let lastPushOverYear =
            map (repoName . fst)
                $ filter (repoOld now days \/ repoSmall commits)
                $ privRepos
    putStrLn $ "All repositories:          " <> show (length $ nub $ map (repoName . fst) $ privRepos)
    putStrLn $ "Old or small repositories: " <> show (length lastPushOverYear)
    rs <- traverse (fromName mgr a org) $ nub $ sort lastPushOverYear
    traverse_ prettyR rs

executeRequestWithMgr :: Manager -> GH.Auth -> GH.Request k a -> IO a
executeRequestWithMgr mgr auth r = do
    x <- GH.executeRequestWithMgr mgr auth r
    either throwM pure x

