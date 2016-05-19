{-# LANGUAGE OverloadedStrings  #-}
module Futurice.App.Futucli.Command.GithubMembersNoTeam (githubMembersNoTeam) where

import Prelude        ()
import Prelude.Compat

import Control.Monad.Catch     (throwM)
import Data.Foldable           (traverse_)
import Data.Maybe              (fromMaybe)
import Data.Monoid             ((<>))
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO               (stderr)

import qualified Data.Text.IO as T
import qualified Data.Vector  as V
import qualified Data.Set as Set

import qualified GitHub as GH

import Futurice.App.Futucli.Cfg

githubMembersNoTeam :: Cfg -> IO ()
githubMembersNoTeam cfg = do
    mgr <- newManager tlsManagerSettings
    users <- executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.membersOfWithR (_cfgGhOrg cfg) GH.OrgMemberFilterAll GH.OrgMemberRoleAll Nothing
    teams <- executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.teamsOfR (_cfgGhOrg cfg) Nothing
    let f t = do
          T.hPutStrLn stderr $ "Team: " <> GH.simpleTeamName t
          executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.listTeamMembersR (GH.simpleTeamId t) GH.TeamMemberRoleAll Nothing
    teamsMembers <- traverse f teams
    let usersSet = Set.fromList $ V.toList users
    let usersInTeams = Set.fromList $ concatMap V.toList $ V.toList teamsMembers
    let orphans = usersSet `Set.difference` usersInTeams
    traverse_ (printSimpleUser mgr (_cfgGhToken cfg)) orphans

printSimpleUser :: Manager -> GH.Auth -> GH.SimpleUser -> IO ()
printSimpleUser mgr auth u = do
    user <- executeRequestWithMgr mgr auth $ GH.userInfoForR (GH.simpleUserLogin u)
    T.putStrLn $ GH.untagName (GH.simpleUserLogin u) <> ": " <> fromMaybe "" (GH.userName user)

executeRequestWithMgr :: Manager -> GH.Auth -> GH.Request k a -> IO a
executeRequestWithMgr mgr auth r = do
    x <- GH.executeRequestWithMgr mgr auth r
    either throwM pure x
