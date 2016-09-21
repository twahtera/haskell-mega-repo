{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.Types (
    -- * Project
    Project (..),
    projectId,
    projectName,
    projectClosed,
    -- * Ctx
    Ctx (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

import qualified PlanMill as PM

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

data Project = Project
    { _projectId     :: PM.ProjectId
    , _projectName   :: !Text
--  ,  tasks     :: [Task]
    , _projectClosed :: !Bool
    }
  deriving (Eq, Show, Typeable, Generic)
  
makeLenses ''Project
deriveGeneric ''Project

instance Arbitrary Project where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON Project where toJSON = sopToJSON
instance FromJSON Project where parseJSON = sopParseJSON
instance ToSchema Project where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
