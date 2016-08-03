{-# LANGUAGE OverloadedStrings #-}
-- | High-level query api
module PlanMill.Queries (
    MonadPlanMillQuery,
    timereports,
    capacities,
    users,
    ) where

import PlanMill.Internal.Prelude

import Control.Monad.PlanMill

import PlanMill.Types.Query          (Query (..), QueryTag (..))
import PlanMill.Types.ResultInterval (Interval)
import PlanMill.Types.Timereport     (Timereports)
import PlanMill.Types.UrlPart        (toUrlParts)
import PlanMill.Types.User           (UserId, Users)
import PlanMill.Types.UserCapacity   (UserCapacities)

-- | Get timereports for interval and user.
timereports :: MonadPlanMillQuery m => Interval Day -> UserId -> m Timereports
timereports i u = planmillVectorQuery (QueryTimereports i u)

-- | Get capacities for interval and user.
capacities :: MonadPlanMillQuery m => Interval Day -> UserId -> m UserCapacities
capacities i u = planmillVectorQuery (QueryCapacities i u)

-- | Get all users.
users :: MonadPlanMillQuery m => m Users
users = planmillVectorQuery
    $ QueryPagedGet QueryTagUser []
    $ toUrlParts ("users" :: Text)
