{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Common integrations requests.
module Futurice.Integrations.Common (
    -- * Date
    beginningOfCurrMonth,
    beginningOfPrevMonth,
    beginningOfPrev2Month,
    -- * FUM
    fumEmployeeList,
    flowdockOrganisation,
    githubOrganisationMembers,
    -- * PlanMill
    fumPlanmillMap,
    planmillEmployee,
    -- * Classy lenses
    HasFUMEmployeeListName(..),
    HasFlowdockOrgName(..),
    HasGithubOrgName(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Time
       (addGregorianMonthsClip, fromGregorian, toGregorian)
import Futurice.Integrations.Classes
import Futurice.Integrations.Types
import Futurice.IdMap (IdMap, idMapOf)
import Text.Regex.Applicative.Text   (anySym, match)

import qualified Data.HashMap.Strict as HM

import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH
import qualified PlanMill           as PM
import qualified PlanMill.Queries   as PMQ

class HasFUMEmployeeListName a where
    fumEmployeeListName :: Lens' a FUM.ListName

class HasFlowdockOrgName a where
    flowdockOrganisationName :: Lens' a (FD.ParamName FD.Organisation)

class HasGithubOrgName a where
    githubOrganisationName :: Lens' a (GH.Name GH.Organization)

-- |
--
-- >>> beginningOfCurrMonth $(mkDay "2016-11-12")
-- 2016-11-01
beginningOfCurrMonth :: Day -> Day
beginningOfCurrMonth = fromGregorian' . f. toGregorian
  where
    f (y, m, _) = (y, m, 1)

    fromGregorian' :: (Integer, Int, Int) -> Day
    fromGregorian' (y, m, d) = fromGregorian y m d

-- |
--
-- >>> beginningOfPrevMonth $(mkDay "2016-11-12")
-- 2016-10-01
beginningOfPrevMonth :: Day -> Day
beginningOfPrevMonth = addGregorianMonthsClip (-1) . beginningOfCurrMonth

-- |
--
-- >>> beginningOfPrev2Month $(mkDay "2016-11-12")
-- 2016-09-01
beginningOfPrev2Month :: Day -> Day
beginningOfPrev2Month = addGregorianMonthsClip (-2) . beginningOfCurrMonth

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

-- | Get all members of the organisation.
githubOrganisationMembers
    :: ( MonadGitHub m
       , MonadGitHubC m (Vector GH.SimpleUser)
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.SimpleUser)
githubOrganisationMembers = do
    orgName <- view githubOrganisationName
    githubReq $ GH.membersOfR orgName GH.FetchAll

-- | Get a mapping fum username to planmill user
--
-- Silently drops FUM users, which we cannot find planmill user for.
fumPlanmillMap
    :: ( MonadFUM m, MonadPlanMillQuery m
       , MonadReader env m, HasFUMEmployeeListName env
       )
    => m (HashMap FUM.UserName (FUM.User, PM.User))
fumPlanmillMap =
    combine <$> fumEmployeeList <*> users
  where
    users = do
        us <- PMQ.users
        traverse (PMQ.user . view PM.identifier) us

    combine :: Vector FUM.User -> Vector PM.User -> HashMap FUM.UserName (FUM.User, PM.User)
    combine fum pm = HM.fromList $ catMaybes $ map extract $ toList pm
      where
        fumNames :: IdMap FUM.User
        fumNames = idMapOf folded fum

        extract :: PM.User -> Maybe (FUM.UserName, (FUM.User, PM.User))
        extract pmUser = do
            name <- match loginRe (PM.uUserName pmUser)
            fumUser <- fumNames ^. at name
            pure (name, (fumUser, pmUser))

        loginRe = FUM.UserName . view packed
            <$  "https://login.futurice.com/openid/"
            <*> many anySym

-- | Get information about employee from planmill
--
-- /TODO/: use applicative
planmillEmployee
    :: MonadPlanMillQuery m
    => PM.UserId
    -> m Employee
planmillEmployee uid = do
    u <- PMQ.user uid
    t <- traverse PMQ.team (PM.uTeam u)
    c <- PMQ.enumerationValue (PM.uContractType u) "Unknown Contract"
    return $ Employee
        { employeeName     = PM.uFirstName u <> " " <> PM.uLastName u
        , employeeTeam     = maybe "Unknown Team" PM.tName t
        , employeeContract = c
        }
