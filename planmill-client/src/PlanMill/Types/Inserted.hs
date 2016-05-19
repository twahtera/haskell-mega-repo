{-# LANGUAGE DataKinds #-}
module PlanMill.Types.Inserted (Inserted) where

import Data.Aeson.Extra          (SingObject (..))
import PlanMill.Types.Identifier (Identifier)
import PlanMill.Types.Timereport (Timereport)

-- | Result type of 'addTimereport'.
type Inserted i = SingObject "id" (Identifier Timereport)
