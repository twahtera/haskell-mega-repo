{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Futucli.Cfg (Cfg(..)) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson (FromJSON (..), withObject, (.:))

import qualified FUM
import qualified GitHub   as GH
import qualified PlanMill as PM
-- import qualified PlanMill.Classes as PM

import Futurice.App.Futucli.Orphans ()

data Cfg = Cfg
    { _cfgGhToken    :: !GH.Auth
    , _cfgGhOrg      :: !(GH.Name GH.Organization)
    , _cfgGhTeam     :: !(GH.Name GH.Team)
    , _cfgFumToken   :: !FUM.AuthToken
    , _cfgFumBaseurl :: !FUM.BaseUrl
    , _cfgFumList    :: !FUM.ListName
    , _cfgPmUserId   :: !PM.UserId
    , _cfgPmToken    :: !PM.ApiKey
    , _cfgPmBaseurl  :: !String
    }
    deriving Show

instance PM.HasPlanMillBaseUrl Cfg where
    getPlanMillBaseUrl = _cfgPmBaseurl

instance PM.HasCredentials Cfg where
    getUserId = _cfgPmUserId
    getApiKey = _cfgPmToken

instance FromJSON Cfg where
    parseJSON = withObject "Config" $ \obj -> Cfg
        <$> obj .: "github-token"
        <*> obj .: "github-org"
        <*> obj .: "github-team"
        <*> obj .: "fum-token"
        <*> obj .: "fum-base-url"
        <*> obj .: "fum-list-name"
        <*> obj .: "planmill-user-id"
        <*> obj .: "planmill-token"
        <*> obj .: "planmill-base-url"
