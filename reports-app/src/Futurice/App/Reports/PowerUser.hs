{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.PowerUser (
    -- * Report
    PowerUserReport,
    powerUserReport,
    -- * Types
    PowerUser (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns
import Data.Vector.Lens (toVectorOf)

import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ
import qualified FUM

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PowerUser = PowerUser
    { _powerUserUsername :: !FUM.UserName
    , _powerUserFirst    :: !Text
    , _powerUserLast     :: !Text
    , _powerUserTeam     :: !Text
    , _powerUserStart    :: !(Maybe Day)
    , _powerUserEnd      :: !(Maybe Day)
    , _powerUserActive   :: !Text
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerUser

instance NFData PowerUser
instance ToColumns PowerUser
instance ToSchema PowerUser where declareNamedSchema = sopDeclareNamedSchema
instance ToJSON PowerUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

-- instance DefaultOrdered PowerUser where headerOrder = sopHeaderOrder
-- instance ToNamedRecord PowerUser where toNamedRecord = sopToNamedRecord

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type PowerUserReport = Report
    "Power: users listing"
    ReportGenerated
    (Vector PowerUser)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

powerUserReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m PowerUserReport
powerUserReport = do
    now <- currentTime
    fpm <- fumPlanmillMap
    fpm' <- toVectorOf folded <$> itraverse powerUser fpm
    pure $ Report (ReportGenerated now) fpm'

powerUser :: MonadPlanMillQuery m => FUM.UserName -> PM.User -> m PowerUser
powerUser fumLogin u = do
    t <- traverse PMQ.team (PM.uTeam u)
    a <- PMQ.enumerationValue (PM.uPassive u) "-"
    pure $ PowerUser
        { _powerUserUsername = fumLogin
        , _powerUserFirst    = PM.uFirstName u
        , _powerUserLast     = PM.uLastName u
        , _powerUserTeam     = maybe "Unknown Team" PM.tName t
        , _powerUserStart    = PM.uHireDate u
        , _powerUserEnd      = PM.uDepartDate u
        , _powerUserActive   = a
        }
