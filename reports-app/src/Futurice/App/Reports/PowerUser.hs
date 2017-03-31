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
import qualified Data.HashMap.Strict as HM
import qualified FUM

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PowerUser = PowerUser
    { _powerUserUsername       :: !FUM.UserName
    , _powerUserFirst          :: !Text
    , _powerUserLast           :: !Text
    , _powerUserTeam           :: !Text
    , _powerUserSupervisor     :: !(Maybe FUM.UserName)
    , _powerUserSupervisorName :: !(Maybe Text)
    , _powerUserStart          :: !(Maybe Day)
    , _powerUserEnd            :: !(Maybe Day)
    , _powerUserActive         :: !Text
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

type PowerUserReport = Vector PowerUser

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

type SupervisorMap = HashMap FUM.UserName (FUM.UserName, Text)

powerUserReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m PowerUserReport
powerUserReport = do
    fpm <- snd <$$> fumPlanmillMap
    supervisorMap <- supervisors <$> fumEmployeeList
    toVectorOf folded <$> itraverse (powerUser supervisorMap) fpm
  where
    -- TODO: move into futurice-integrations, also used in balances
    supervisors :: Vector FUM.User -> SupervisorMap
    supervisors us = HM.fromList . mapMaybe mk $ us'
      where
        us' = toList us

        mk u = do
          superId   <- u ^. FUM.userSupervisor . lazy
          superData <- ss ^? ix superId
          pure (u ^. FUM.userName, superData)

        -- id to name map
        ss :: HashMap Int (FUM.UserName, Text)
        ss = HM.fromList . map mkData $ us'

        mkData :: FUM.User -> (Int, (FUM.UserName, Text))
        mkData u =
            ( u ^. FUM.userId
            , ( u ^. FUM.userName, u ^. FUM.userFullName)
            )

powerUser
    :: MonadPlanMillQuery m
    => SupervisorMap
    -> FUM.UserName -> PM.User -> m PowerUser
powerUser supervisors fumLogin u = do
    t <- traverse PMQ.team (PM.uTeam u)
    a <- PMQ.enumerationValue (PM.uPassive u) "-"
    pure $ PowerUser
        { _powerUserUsername       = fumLogin
        , _powerUserFirst          = PM.uFirstName u
        , _powerUserLast           = PM.uLastName u
        , _powerUserTeam           = maybe "Unknown Team" PM.tName t
        , _powerUserStart          = PM.uHireDate u
        , _powerUserEnd            = PM.uDepartDate u
        , _powerUserActive         = a
        , _powerUserSupervisor     = supervisors ^? ix fumLogin . _1
        , _powerUserSupervisorName = supervisors ^? ix fumLogin . _2
        }

