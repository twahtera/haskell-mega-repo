{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.MissingHours (
    -- * Report
    MissingHoursTitle,
    MissingHoursTitleFilt,
    MissingHoursReport,
    missingHoursReport,
    -- * Data
    MissingHour (..),
    -- * Logic
    missingHoursForUser,
    -- * Lenses
    missingHourDay,
    missingHourCapacity,
    -- ** Params
    mhpGenerated,
    mhpFromDay,
    mhpToDay,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Fixed                (Centi)
import Control.Lens (contains)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Report.Columns
import Futurice.Time
import Numeric.Interval.NonEmpty (inf, sup)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Tuple.Strict   as S
import qualified Data.Vector         as V
import qualified FUM
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data MissingHour = MissingHour
   { _missingHourDay      :: !Day
   , _missingHourCapacity :: !(NDT 'Hours Centi)
   }
    deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''MissingHour
deriveGeneric ''MissingHour

instance NFData MissingHour

instance ToJSON MissingHour where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON MissingHour where parseJSON = sopParseJSON
instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns MissingHour where
    type Columns MissingHour = '[Day, NDT 'Hours Centi]
    toColumns (MissingHour d c) = [I d :* I c :* Nil]

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type MissingHoursTitle = "Missing hour markings"
type MissingHoursTitleFilt = "Missing hour markings, filtered"

type MissingHoursReport title = Report
    title
    MissingHoursParams
    (HashMap FUM.UserName :$ StrictPair Employee :$ Vector :$ MissingHour)

data MissingHoursParams = MissingHoursParams
    { _mhpGenerated    :: !UTCTime
    , _mhpFromDay      :: !Day
    , _mhpToDay        :: !Day
    , _mhpFUMPublicUrl :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''MissingHoursParams
makeLenses ''MissingHoursParams

instance HasFUMPublicURL MissingHoursParams where
    fumPublicUrl = mhpFUMPublicUrl

instance NFData MissingHoursParams
instance ToSchema MissingHoursParams where declareNamedSchema = sopDeclareNamedSchema
instance ToJSON MissingHoursParams where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON MissingHoursParams where
    parseJSON = sopParseJSON

instance ToHtml MissingHoursParams where
    toHtmlRaw = toHtml
    toHtml MissingHoursParams {..} = dl_ $ do
        dd_ $ do
            "Generated at "
            i_ ("(Note: data is pulled from caches, so it is few hours old at worst)")
        dt_ $ toHtml $ formatHumanHelsinkiTime _mhpGenerated

        dd_ "Interval"
        dt_ $ toHtmlRaw $ show _mhpFromDay <> " &mdash; " <> show _mhpToDay

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | TODO: use applicative
--
-- /TODO:/ should return Map Day DoubleMissingHour?
missingHoursForUser
    :: MonadPlanMillQuery m
    => PM.Interval Day
    -> PM.User
    -> m (Vector MissingHour)
missingHoursForUser interval user = do
    let uid = user ^. PM.identifier
    tr <- PMQ.timereports interval uid
    uc <- PMQ.capacities interval uid
    -- Show only missing hours after hireDate.
    -- Contrary to the name, hire date in Planmill isn't always trustworthy,
    -- but it's good enough for the purpose of this report.
    let uc' = V.filter (\c -> fromMaybe True $ (PM.userCapacityDate c >=) <$> PM.uHireDate user) uc
    pure $ mkMissingHours tr uc'
  where
    mkMissingHours :: PM.Timereports -> PM.UserCapacities -> Vector MissingHour
    mkMissingHours tr uc
        = V.fromList
        . map (uncurry MissingHour)
        . Map.toList
        $ Map.differenceWith minus uc' tr'
      where
        tr' :: Map Day (NDT 'Hours Centi)
        tr' = Map.fromListWith (+)
            . map (\x -> (PM.trStart x, ndtConvert' $ PM.trAmount x))
            . toList
            $ tr

        uc' :: Map Day (NDT 'Hours Centi)
        uc' = Map.fromList
            . filter (isPositive . snd)
            . map (\x -> (PM.userCapacityDate x, ndtConvert' $ PM.userCapacityAmount x))
            . toList
            $ uc

    -- For now show only days without any hour markings
    minus :: NDT 'Hours Centi -> NDT 'Hours Centi -> Maybe (NDT 'Hours Centi)
    minus a b
        | b > 0      = Nothing
        | otherwise  = Just a

    isPositive :: (Num a, Ord a) => a -> Bool
    isPositive = (>0)

missingHoursReport
    :: forall m env title.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env, HasFUMPublicURL env
        )
    => Maybe (Set (PM.EnumValue PM.User "contractType"))
    -> PM.Interval Day
    -> m (MissingHoursReport title)
missingHoursReport mcontractTypes interval = do
    fumPubUrl <- view fumPublicUrl
    now <- PM.currentTime
    fpm0 <- snd <$$> fumPlanmillMap
    let fpm1 = case mcontractTypes of
            Just contractTypes ->
                HM.filter (\e -> contractTypes ^. contains (PM.uContractType e)) fpm0
            Nothing ->
                fpm0
    fpm2 <- traverse perUser fpm1
    pure $ Report (MissingHoursParams now (inf interval) (sup interval) fumPubUrl) fpm2
  where
    perUser :: PM.User -> m (StrictPair Employee :$ Vector :$ MissingHour)
    perUser pmUser = (S.:!:)
        <$> planmillEmployee (pmUser ^. PM.identifier)
        <*> missingHoursForUser interval pmUser
