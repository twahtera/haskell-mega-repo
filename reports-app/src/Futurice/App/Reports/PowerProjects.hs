{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.PowerProjects (
    -- * Report
    PowerProjectsReport,
    powerProjectsReport,
    -- * Types
    PowerAccount (..),
    PowerProject (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns
import Data.Vector.Lens (vector)
import Data.Set.Lens (setOf)
import Control.Lens (to)

import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PowerProject = PowerProject
    { _powerProjectId        :: !PM.ProjectId
    , _powerProjectAccountId :: !(Maybe PM.AccountId)
    , _powerProjectName      :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerProject

instance NFData PowerProject
instance ToColumns PowerProject
instance ToSchema PowerProject where declareNamedSchema = sopDeclareNamedSchema
instance ToJSON PowerProject where
    toJSON = sopToJSON
    toEncoding = sopToEncoding


data PowerAccount = PowerAccount
    { _powerAccountId        :: !PM.AccountId
    , _powerAccountName      :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerAccount

instance NFData PowerAccount
instance ToColumns PowerAccount
instance ToSchema PowerAccount where declareNamedSchema = sopDeclareNamedSchema
instance ToJSON PowerAccount where
    toJSON = sopToJSON
    toEncoding = sopToEncoding


-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

data PowerProjectsReport = PowerProjectsReport
    { powerProjects :: !(Vector PowerProject)
    , powerAccounts :: !(Vector PowerAccount)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerProjectsReport

instance NFData PowerProjectsReport
instance ToColumns PowerProjectsReport
instance ToSchema PowerProjectsReport where declareNamedSchema = sopDeclareNamedSchema
instance ToJSON PowerProjectsReport where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

powerProjectsReport
    :: forall m.
        ( MonadPlanMillQuery m
        )
    => m PowerProjectsReport
powerProjectsReport = do
    ps <- PMQ.projects
    -- we don't have query for all accounts, but it's almost as efficient
    -- to fetch based on aids list.
    --
    -- "Benefit": we will have only the accounts with projects
    --
    let aids = toList (setOf (folded . to PM.pAccount . folded) ps)
    as <- traverse PMQ.account aids 
    pure $ PowerProjectsReport
        { powerProjects = toPower <$> ps
        , powerAccounts = toPowerA <$> as ^. vector
        }
  where
    toPower :: PM.Project -> PowerProject
    toPower p = PowerProject
        { _powerProjectId        = p ^. PM.identifier
        , _powerProjectAccountId = PM.pAccount p
        , _powerProjectName      = PM.pName p
        }

    toPowerA :: PM.Account -> PowerAccount
    toPowerA a = PowerAccount
        { _powerAccountId = a ^. PM.identifier
        , _powerAccountName = PM.saName a
        }
