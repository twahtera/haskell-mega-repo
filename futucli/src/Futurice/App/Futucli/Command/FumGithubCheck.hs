{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Futucli.Command.FumGithubCheck (fumGithubCheck) where

import Prelude        ()
import Prelude.Compat

import Control.Lens
import Control.Monad.Catch     (throwM)
import Data.Foldable           (traverse_)
import Data.Monoid             ((<>))
import Data.Proxy              (Proxy (..))
import Data.Text               (Text)
import Data.Vector             (Vector)
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO               (stderr)

import qualified Data.HashSet as HS
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector  as V

import qualified FUM
import qualified GitHub as GH

import Futurice.App.Futucli.Cfg

logic :: Vector GH.SimpleUser -> Vector FUM.User -> IO ()
logic gh fum = do
    let fum' = V.filter (not . isInGH) fum
    T.putStrLn $ "Has GitHub name in FUM, not in GitHub organisation (" <> tshow (V.length fum') <> "):"
    traverse_ (T.putStrLn . showFumUser) fum'
    T.putStrLn ""
    let gh' = V.filter (not . isInFum) gh
    T.putStrLn $ "In GitHub organisation, no username in FUM (" <> tshow (V.length gh') <> "):"
    traverse_ (T.putStrLn . showGithubUser) gh'
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show

    showFumUser :: FUM.User -> Text
    showFumUser u = u ^. FUM.userFirst <> " " <> u ^. FUM.userLast

    showGithubUser :: GH.SimpleUser -> Text
    showGithubUser = GH.untagName . GH.simpleUserLogin

    inGh :: HS.HashSet (GH.Name GH.User)
    inGh = HS.fromList $ V.toList $ V.map GH.simpleUserLogin gh

    isInGH :: FUM.User -> Bool
    isInGH u = case u ^. FUM.userGithub . lazy of
      Nothing       -> True -- Not true :S
      Just ghLogin  -> GH.mkName (Proxy :: Proxy GH.User) ghLogin `HS.member` inGh

    inFum :: HS.HashSet (GH.Name GH.User)
    inFum = HS.fromList $ fum ^.. traverse . FUM.userGithub . lazy . _Just . to (GH.mkName (Proxy :: Proxy GH.User))

    isInFum :: GH.SimpleUser -> Bool
    isInFum u = GH.simpleUserLogin u `HS.member` inFum

fumGithubCheck :: Cfg -> IO ()
fumGithubCheck cfg = do
    let team = _cfgGhTeam cfg
    mgr <- newManager tlsManagerSettings
    teams <- executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.teamsOfR (_cfgGhOrg cfg) Nothing
    case V.find ((team ==) . GH.mkTeamName . GH.simpleTeamName) teams of
        Nothing -> do
            T.hPutStrLn stderr $ "Cannot find team " <> GH.untagName team <> ", printed all users"
        Just t  -> do
            teamUsers <- executeRequestWithMgr mgr (_cfgGhToken cfg) $ GH.listTeamMembersR (GH.simpleTeamId t) GH.TeamMemberRoleAll Nothing
            fumUsers <- FUM.fetchList mgr (_cfgFumToken cfg) (_cfgFumBaseurl cfg) (_cfgFumList cfg)
            logic teamUsers fumUsers

executeRequestWithMgr :: Manager -> GH.Auth -> GH.Request k a -> IO a
executeRequestWithMgr mgr auth r = do
    x <- GH.executeRequestWithMgr mgr auth r
    either throwM pure x
