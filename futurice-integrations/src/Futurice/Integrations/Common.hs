{-# LANGUAGE ConstraintKinds #-}
-- | Common integrations requests.
module Futurice.Integrations.Common (
    -- * FUM
    fumEmployeeList,
    flowdockOrganisation,
    githubOrganisationMembers,
    -- * Classy lenses
    HasFUMEmployeeListName(..),
    HasFlowdockOrgName(..),
    HasGithubOrgName(..),
    ) where

import Futurice.Integrations.Classes
import Futurice.Prelude

import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH

class HasFUMEmployeeListName a where
    fumEmployeeListName :: Lens' a FUM.ListName

class HasFlowdockOrgName a where
    flowdockOrganisationName :: Lens' a (FD.ParamName FD.Organisation)

class HasGithubOrgName a where
    githubOrganisationName :: Lens' a (GH.Name GH.Organization)

-- | Get list of active employees from FUM.
fumEmployeeList
    :: ( MonadFUM m
       , MonadReader env m, HasFUMEmployeeListName env
       )
    => m (Vector FUM.User)
fumEmployeeList = do
    listName <- view fumEmployeeListName
    FUM.fumList listName

-- | Get organisation from Flowdock
flowdockOrganisation
    :: (MonadFlowdock m, MonadReader env m, HasFlowdockOrgName env)
    => m FD.Organisation
flowdockOrganisation = do
    orgName <- view flowdockOrganisationName
    flowdockOrganisationReq orgName

githubOrganisationMembers
    :: ( MonadGitHub m
       , MonadGitHubC m (Vector GH.SimpleUser)
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.SimpleUser)
githubOrganisationMembers = do
    orgName <- view githubOrganisationName
    githubReq $ GH.membersOfR orgName GH.FetchAll
