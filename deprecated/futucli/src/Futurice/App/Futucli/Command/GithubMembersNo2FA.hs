{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Futucli.Command.GithubMembersNo2FA (githubMembersNo2FA) where

import Futurice.Prelude

import System.IO (stderr)

import qualified Data.Text.IO as T
import qualified Data.Vector  as V

import qualified GitHub as GH

import Futurice.App.Futucli.Cfg

githubMembersNo2FA :: Cfg -> IO ()
githubMembersNo2FA cfg = do
    let team = _cfgGhTeam cfg
    mgr <- newManager tlsManagerSettings
    users <- executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.membersOfWithR (_cfgGhOrg cfg) GH.OrgMemberFilter2faDisabled GH.OrgMemberRoleAll GH.FetchAll
    teams <- executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.teamsOfR (_cfgGhOrg cfg) GH.FetchAll
    case V.find ((team ==) . GH.mkTeamName . GH.simpleTeamName) teams of
        Nothing -> do
            T.hPutStrLn stderr $ "In total " <> textShow (V.length users) <> " users without 2fa"
            traverse_ (printSimpleUser mgr (_cfgGhToken cfg)) users
            T.hPutStrLn stderr $ "Cannot find team " <> GH.untagName team <> ", printed all users"
        Just t  -> do
            teamUsers <- executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.listTeamMembersR (GH.simpleTeamId t) GH.TeamMemberRoleAll GH.FetchAll
            let teamUsersIds = GH.simpleUserId <$> teamUsers
            let users' = V.filter (\u -> GH.simpleUserId u `V.elem` teamUsersIds) users
            T.hPutStrLn stderr $ "There are " <> textShow (V.length teamUsers) <> " users in the team"
            T.hPutStrLn stderr $ "There are " <> textShow (V.length users') <> " users without 2fa"
            traverse_ (printSimpleUser mgr (_cfgGhToken cfg)) users'

printSimpleUser :: Manager -> GH.Auth -> GH.SimpleUser -> IO ()
printSimpleUser mgr auth u = do
    user <- executeRequestWithMgr mgr auth $ GH.userInfoForR (GH.simpleUserLogin u)
    T.putStrLn $ GH.untagName (GH.simpleUserLogin u) <> ": " <> fromMaybe "" (GH.userName user)

executeRequestWithMgr :: Manager -> GH.Auth -> GH.Request k a -> IO a
executeRequestWithMgr mgr auth r = do
    x <- GH.executeRequestWithMgr mgr auth r
    either throwM pure x
