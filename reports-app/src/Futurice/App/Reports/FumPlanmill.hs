{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | FUM
module Futurice.App.Reports.FumPlanmill (
    -- * Report
    FumPlanmillReport,
    fumPlanmillReport,
    -- * Types
    PlanmillUser (..),
    FUMUser (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens (to)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns
import Text.Regex.Applicative.Text   (anySym, match)

import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ
import qualified FUM

type Login = Text
type Status = Text

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PlanmillUser = PlanmillUser
    { _pmName      :: !Text
    , _pmLogin     :: !Text
    , _pmStatus    :: !Status
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

data FUMUser = FUMUser
    { _fumUserName  :: !Text
    , _fumUserLogin :: !Text
    , _fumStatus    :: !Status
    , _fumPmStatus  :: !Status
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

--makeLenses ''PlanmillUser
--makeLenses ''FUMUser

deriveGeneric ''PlanmillUser
deriveGeneric ''FUMUser

instance NFData PlanmillUser
instance NFData FUMUser

instance ToJSON PlanmillUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToJSON FUMUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToSchema PlanmillUser where
    declareNamedSchema = sopDeclareNamedSchema
instance ToSchema FUMUser where
    declareNamedSchema = sopDeclareNamedSchema

instance ToColumns PlanmillUser where
    columnNames _ =
        K "pm-name" :*
        K "pm-login" :*
        K "pm-status" :*
        Nil

instance ToColumns FUMUser where
    columnNames _ =
        K "fum-name" :*
        K "fum-login" :*
        K "fum-status" :*
        K "fum-pm-status" :*
        Nil

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type FumPlanmillReport = Report
    "Users in FUM ↔ Planmill"
    ReportGenerated
    [These FUMUser PlanmillUser]

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

type Enums = Map (PM.EnumValue PM.User "passive") Text

fumPlanmillReport
    :: forall m env.
        ( MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m FumPlanmillReport
fumPlanmillReport = do
    now <- currentTime
    fs <- fumEmployeeList
    enums <- PMQ.allEnumerationValues (Proxy :: Proxy PM.User) (Proxy :: Proxy "passive")
    pms <- PMQ.users
    return
        $ Report (ReportGenerated now)
        $ sortOn metric
        $ makeReport enums fs pms
  where
    makeReport
        :: Enums
        -> Vector FUM.User
        -> Vector PM.User
        -> [These FUMUser PlanmillUser]
    makeReport enums fs pms = toList (align fs' pms')
      where
        fs' :: Map Login FUMUser
        fs'  = toMapOf (folded . to (toFUMUser enums). ifolded) fs
        pms' :: Map Login PlanmillUser
        pms' = toMapOf (folded . to (toPMUser enums) . ifolded) pms

    metric :: These FUMUser PlanmillUser -> Int
    -- in fum and pm, but inactive in PM
    metric (These _ u) | _pmStatus u == "Passive"    = 1
    -- Only in FUM
    metric (This _)    | otherwise                   = 2
    -- will be disabled manually at some point
    metric (That u)    | _pmStatus u == "Active"     = 3
    -- normal users
    metric (These _ u) | _pmStatus u == "Active"     = 4
    -- old disabled employees
    metric (That u)    | _pmStatus u == "Passive"    = 5
    -- Should not happen
    metric (These _ _) | otherwise                   = 0
    metric (That _)    | otherwise                   = 0

    toFUMUser :: Enums -> FUM.User -> (Login, FUMUser)
    toFUMUser _enums u = (_fumUserLogin u', u')
      where
        u' = FUMUser
            { _fumUserName  = u ^. FUM.userFirst <> " " <> u ^. FUM.userLast
            , _fumUserLogin = u ^. FUM.userName ^. FUM.getUserName
            , _fumStatus    = textShow $ u ^. FUM.userStatus
            , _fumPmStatus  = textShow $ u ^. FUM.userActiveInPm
            }

    toPMUser :: Enums -> PM.User -> (Login, PlanmillUser)
    toPMUser enums u = (_pmLogin u', u')
      where
        u' = PlanmillUser
            { _pmName   = PM.uFirstName u <> " " <> PM.uLastName u
            -- this should always match, but use user-name field if it doesn't
            , _pmLogin  = fromMaybe (PM.uUserName u) $
                match loginRe (PM.uUserName u)
            , _pmStatus = fromMaybe "unknown" $ enums ^? ix (PM.uPassive u)
            }

    loginRe = view packed
        <$  "https://login.futurice.com/openid/"
        <*> many anySym


