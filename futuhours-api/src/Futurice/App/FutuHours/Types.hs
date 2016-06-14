{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.FutuHours.Types (
    Project(..),
    UserId(..),
    FUMUsername(..),
    FUMUsernamesParam(..),
    PlanmillApiKey(..),
    PlanmillUserLookupTable,
    PlanmillUserIdLookupTable,
    Timereport(..),
    Envelope(..),
    User(..),
    Hour(..),
    -- * Reports
    PerEmployee(..),
    -- ** Missing hours
    MissingHoursReport,
    MissingHour(..),
    -- ** Balance
    BalanceReport,
    Balance(..),
    -- * Power
    PowerUser(..),
    PowerAbsence(..),
    -- * Precalculated endpoints
    EndpointTag(..),
    -- * Flags
    Development(..),
    -- * Random
    reverseLookup,
    ) where

import Futurice.Prelude

import Data.Aeson.Extra   (FromJSON (..), M, ToJSON (..), Value (..), object,
                           withObject, (.:), (.=))
import Data.Csv           (DefaultOrdered (..), ToField (..),
                           ToNamedRecord (..))
import Data.GADT.Compare  ((:~:) (..), GCompare (..), GEq (..), GOrdering (..))
import Data.Swagger       (ToParamSchema, ToSchema (..))
import Futurice.EnvConfig (FromEnvVar (..))
import Futurice.Generics  (sopDeclareNamedSchema, sopHeaderOrder, sopParseJSON,
                           sopToJSON, sopToNamedRecord)
import Lucid              hiding (for_)
import Servant            (Capture, FromHttpApiData (..))
import Servant.Docs       (DocCapture (..), ToCapture (..))

import qualified Futurice.IC     as IList
import           Futurice.Peano
import           Futurice.Report

import qualified Data.Aeson                           as Aeson
import qualified Data.Aeson.Types                     as Aeson
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Set                             as Set
import qualified Data.Swagger                         as Swagger
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified PlanMill                             as PM

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

camelTo :: String -> String
#if MIN_VERSION_aeson(0,10,0)
camelTo = Aeson.camelTo2 '_'
#else
camelTo = Aeson.camelTo '_'
#endif

-------------------------------------------------------------------------------
-- Development
-------------------------------------------------------------------------------

data Development
    = Development
    | Production
    deriving (Eq, Show)

instance FromEnvVar Development where
    fromEnvVar = fmap f . fromEnvVar
      where
        f True  = Development
        f False = Production

-------------------------------------------------------------------------------
-- UserId - deprecated
-------------------------------------------------------------------------------

newtype UserId = UserId Int
  deriving (Generic) -- TODO: needed for ToParamSchema

instance ToParamSchema UserId

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "PlanMill userid"

instance FromHttpApiData UserId where
    parseUrlPiece = fmap UserId . parseUrlPiece

-------------------------------------------------------------------------------
-- FUMUsername
-------------------------------------------------------------------------------

newtype FUMUsername = FUMUsername Text
    deriving (Eq, Ord, Show, Typeable, Generic)

getFUMUsername :: FUMUsername -> Text
getFUMUsername (FUMUsername name) = name

instance ToJSON FUMUsername where
    toJSON (FUMUsername n) = toJSON n
#if MIN_VERSION_aeson(0,10,0)
    toEncoding (FUMUsername n) = toEncoding n
#endif
instance ToSchema FUMUsername
instance ToParamSchema FUMUsername

instance ToSchema a => ToSchema (HashMap FUMUsername a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (HashMap String a))

hmMapKey :: (Eq k', Hashable k') => (k -> k') -> HashMap k v -> HashMap k' v
hmMapKey f = HM.fromList . map (first f) . HM.toList

instance ToJSON a => ToJSON (HashMap FUMUsername a) where
    toJSON = toJSON . hmMapKey getFUMUsername

instance ToJSON1 (HashMap FUMUsername) where
    liftToJSON t = toJSON . fmap t

instance FromJSON1 (HashMap FUMUsername) where
    liftParseJSON p v = parseJSON v >>= traverse p

instance FromJSON a => FromJSON (HashMap FUMUsername a) where
    parseJSON = fmap (hmMapKey FUMUsername) . parseJSON

instance ToCapture (Capture "fum-id" FUMUsername) where
    toCapture _ = DocCapture "fum-id" "FUM username"

instance FromHttpApiData FUMUsername where
    parseUrlPiece = fmap FUMUsername . parseUrlPiece

instance Postgres.ToField FUMUsername where
    toField (FUMUsername name) = Postgres.toField name

instance Postgres.FromField FUMUsername where
    fromField f bs = FUMUsername <$> Postgres.fromField f bs

instance Hashable FUMUsername

instance ToField FUMUsername where
    toField = toField.  getFUMUsername

instance ToHtml FUMUsername where
    toHtml = toHtml . getFUMUsername
    toHtmlRaw = toHtmlRaw . getFUMUsername

-- | List of users
newtype FUMUsernamesParam = FUMUsernamesParam
    { getFUMUsernamesParam :: [FUMUsername] }
  deriving (Eq)

instance FromHttpApiData FUMUsernamesParam where
    parseUrlPiece = Right . FUMUsernamesParam . map FUMUsername . T.words

instance ToParamSchema FUMUsernamesParam where
    toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy Text) -- TODO: pattern

-------------------------------------------------------------------------------
-- PlanmillApiKey
-------------------------------------------------------------------------------

newtype PlanmillApiKey = PlanmillApiKey Text
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON PlanmillApiKey where
    toJSON (PlanmillApiKey k) = toJSON k
#if MIN_VERSION_aeson(0,10,0)
    toEncoding (PlanmillApiKey k) = toEncoding k
#endif
instance FromJSON PlanmillApiKey
instance ToSchema PlanmillApiKey

instance Postgres.ToField PlanmillApiKey where
    toField (PlanmillApiKey key) = Postgres.toField key

instance Postgres.FromField PlanmillApiKey where
    fromField f bs = PlanmillApiKey <$> Postgres.fromField f bs

type PlanmillUserLookupTable = HM.HashMap FUMUsername PM.User
type PlanmillUserIdLookupTable = HM.HashMap FUMUsername PM.UserId

reverseLookup :: (Eq v, Hashable v) => v -> HM.HashMap k v -> Maybe k
reverseLookup pid hm = HM.lookup pid revHm
  where
    revHm = HM.fromList . map swap . HM.toList $ hm
    swap (a, b) = (b, a)

-------------------------------------------------------------------------------
-- Timereport
-------------------------------------------------------------------------------

data Timereport = Timereport
    { timereportId      :: !PM.TimereportId
    , timereportComment :: !(Maybe Text)
    }
    deriving (Generic)

deriveGeneric ''Timereport

instance ToJSON Timereport where toJSON = sopToJSON
instance ToSchema Timereport where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

data Project = Project
    { projectId   :: !PM.ProjectId
    , projectName :: !Text
    }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance Hashable Project

deriveGeneric ''Project

instance ToJSON Project where toJSON = sopToJSON
instance ToSchema Project where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Envelope
-------------------------------------------------------------------------------

newtype Envelope a = Envelope { fromEnvelope :: Vector a }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance ToJSON a => ToJSON (Envelope a) where
    toJSON (Envelope x) = object
        [ "meta" .= object
            [ "next"        .= Null
            , "previous"    .= Null
            , "offset"      .= (0 :: Int)
            , "total_count" .= length x
            , "limit"       .= (1000 :: Int)
            ]
        , "objects" .= x
        ]

instance ToSchema a => ToSchema (Envelope a) where

-------------------------------------------------------------------------------
-- Legacy user
-------------------------------------------------------------------------------

data User = User
    { userFirstName        :: !Text
    , userDefaultWorkHours :: !Double
    , userHolidaysDaysLeft :: !Int
    , userBalance          :: !Int
    , userEmployeeType     :: !Text
    }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance ToJSON User where
  toJSON = Aeson.genericToJSON opts
      where
        opts = Aeson.defaultOptions
            { Aeson.fieldLabelModifier = camelTo . drop 4
            }

instance ToSchema User where
    declareNamedSchema = Swagger.genericDeclareNamedSchema opts
      where
        opts = Swagger.defaultSchemaOptions
            { Swagger.fieldLabelModifier = camelTo . drop 4
            }

-------------------------------------------------------------------------------
-- Legacy hour marking
-------------------------------------------------------------------------------

data Hour = Hour
    { hourAbsence         :: !Bool
    , hourBillable        :: !Bool
    , hourDay             :: !Day
    , hourDescription     :: !Text
    , hourEditable        :: !Bool
    , hourHours           :: !Double
    , hourId              :: !PM.TimereportId
    , hourProjectId       :: !PM.ProjectId
    , hourProjectCategory :: !Int
    , hourProjectName     :: !Text
    , hourStatus          :: !Int
    , hourTaskId          :: !PM.TaskId
    , hourTaskName        :: !Text
    , hourUserId          :: !PM.UserId
    , hourUser            :: !Text
    }
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance ToJSON Hour where
  toJSON = Aeson.genericToJSON opts
      where
        opts = Aeson.defaultOptions
            { Aeson.fieldLabelModifier = camelTo . drop 4
            }

instance ToSchema Hour where
    declareNamedSchema = Swagger.genericDeclareNamedSchema opts
      where
        opts = Swagger.defaultSchemaOptions
            { Swagger.fieldLabelModifier = camelTo . drop 4
            }

-------------------------------------------------------------------------------
-- Reports
-------------------------------------------------------------------------------

-- | Todo move to @futurice-integrations@
data PerEmployee a = PerEmployee
    { perEmployeeName     :: !Text
    , perEmployeeTeam     :: !Text
    , perEmployeeContract :: !Text
    , perEmployeeData     :: !a
    }
    deriving (Functor, Foldable, Traversable)

instance ToReportRow1 PerEmployee where
    type ReportRowLen1 PerEmployee n = 'PS ('PS ('PS n))

    liftReportHeader _ f _ = ReportHeader
        $ IList.cons "name"
        $ IList.cons "team"
        $ IList.cons "contract"
        $ getReportHeader (f Proxy)

    liftReportRow _ f (PerEmployee n t c d) = map prepend $ f d
      where
        prepend = overReportRow
            $ IList.cons (toHtml n)
            . IList.cons (toHtml t)
            . IList.cons (toHtml c)

instance ToJSON1 PerEmployee where
    liftToJSON toJ (PerEmployee n t c d) = object
        [ "name"     .= n
        , "team"     .= t
        , "contract" .= c
        , "data"     .= toJ d
        ]

instance FromJSON1 PerEmployee where
    liftParseJSON fr = withObject "PerEmployee" $ \obj -> PerEmployee
        <$> obj .: "name"
        <*> obj .: "team"
        <*> obj .: "contract"
        <*> (obj .: "data" >>= fr)

-------------------------------------------------------------------------------
-- Balance
-------------------------------------------------------------------------------

data Balance = Balance
    { balanceHours        :: !Double
    , balanceMissingHours :: !Double
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToReportRow Balance where
    type ReportRowLen Balance = 'PS ('PS ('PS 'PZ))

    reportHeader _ = ReportHeader
        $ IList.cons "hours"
        $ IList.cons "missing"
        $ IList.cons "difference"
        $ IList.nil

    reportRow (Balance hours missing) = [r]
      where
        diff = hours + missing
        cls | diff <= -20 || diff >= 40   = "emphasize"
            | hours <= -20 || hours >= 40 = "emphasize2"
            | otherwise                   = "normal"

        r = ReportRow (Set.singleton cls)
            $ IList.cons (toHtml $ show hours)
            $ IList.cons (toHtml $ show missing)
            $ IList.cons (toHtml $ show diff)
            $ IList.nil

deriveGeneric ''Balance

instance ToJSON Balance where toJSON = sopToJSON
instance FromJSON Balance where parseJSON = sopParseJSON
instance ToSchema Balance where declareNamedSchema = sopDeclareNamedSchema

type BalanceReport = Report
    "Balance"
    ReportGenerated
    '[Vector, PerEmployee]
    Balance

-------------------------------------------------------------------------------
-- Missing hours
-------------------------------------------------------------------------------

type MissingHoursReport = Report
    "Missing hours"
    ReportGenerated
    '[HashMap FUMUsername, PerEmployee, Vector]
    MissingHour

data MissingHour = MissingHour
   { missingHourDay      :: !Day
   , missingHourCapacity :: !Double
   }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToReportRow MissingHour where
    type ReportRowLen MissingHour = 'PS ('PS 'PZ)

    reportHeader _ = ReportHeader
        $ IList.cons "day"
        $ IList.cons "capacity"
        $ IList.nil

    reportRow (MissingHour d c) = [r]
      where
        r = ReportRow Set.empty
            $ IList.cons (toHtml $ show d)
            $ IList.cons (toHtml $ show c)
            $ IList.nil

deriveGeneric ''MissingHour

instance ToJSON MissingHour where toJSON = sopToJSON
instance FromJSON MissingHour where parseJSON = sopParseJSON
instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema
--instance DefaultOrdered MissingHour where headerOrder = sopHeaderOrder
--instance ToNamedRecord MissingHour where toNamedRecord = sopToNamedRecord
--instance FromRecord MissingHour where parseRecord = sopParseRecord

-------------------------------------------------------------------------------
-- Power
-------------------------------------------------------------------------------

data PowerUser = PowerUser
    { powerUserUsername :: !FUMUsername
    , powerUserFirst    :: !Text
    , powerUserLast     :: !Text
    , powerUserTeam     :: !Text
    , powerUserStart    :: !(Maybe Day)
    , powerUserEnd      :: !(Maybe Day)
    , powerUserActive   :: !Text
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerUser

instance DefaultOrdered PowerUser where headerOrder = sopHeaderOrder
instance ToNamedRecord PowerUser where toNamedRecord = sopToNamedRecord
instance ToJSON PowerUser where toJSON = sopToJSON
instance ToSchema PowerUser where declareNamedSchema = sopDeclareNamedSchema

data PowerAbsence = PowerAbsence
    { powerAbsenceUsername     :: !(Maybe FUMUsername)
    , powerAbsenceStart        :: !Day
    , powerAbsenceEnd          :: !Day
    , powerAbsencePlanmillId   :: !PM.AbsenceId
    , powerAbsenceCapacities   :: !(M (Map Day Double))
    , powerAbsenceBusinessDays :: !Int
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''PowerAbsence

instance DefaultOrdered PowerAbsence where headerOrder = sopHeaderOrder
instance ToNamedRecord PowerAbsence where toNamedRecord = sopToNamedRecord
instance ToJSON PowerAbsence where toJSON = sopToJSON
instance ToSchema PowerAbsence where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Precalculated Endpoints
-------------------------------------------------------------------------------

data EndpointTag a where
    EMissingHours      :: EndpointTag MissingHoursReport
    -- missing hours from beginning of the previous month till today
    EPowerUsers       :: EndpointTag (Vector PowerUser)
    -- Users in planmill with some additional information
    EPowerAbsences    :: EndpointTag (Vector PowerAbsence)
    -- Absences in next 365 days
    EBalanceReport    :: EndpointTag BalanceReport
    deriving (Typeable)

instance GEq EndpointTag where
    geq EMissingHours     EMissingHours     = Just Refl
    geq EPowerUsers       EPowerUsers       = Just Refl
    geq EPowerAbsences    EPowerAbsences    = Just Refl
    geq EBalanceReport    EBalanceReport    = Just Refl
    geq _ _ = Nothing

instance GCompare EndpointTag where
    gcompare EMissingHours     EMissingHours = GEQ
    gcompare EMissingHours     _                 = GLT
    gcompare _                 EMissingHours = GGT
    gcompare EPowerUsers       EPowerUsers       = GEQ
    gcompare EPowerUsers       _                 = GLT
    gcompare _                 EPowerUsers       = GGT
    gcompare EPowerAbsences    EPowerAbsences    = GEQ
    gcompare EPowerAbsences    _                 = GLT
    gcompare _                 EPowerAbsences    = GGT
    gcompare EBalanceReport    EBalanceReport    = GEQ

deriving instance Show (EndpointTag a)
