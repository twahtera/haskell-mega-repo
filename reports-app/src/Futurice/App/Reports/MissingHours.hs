{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.MissingHours (
    -- * Report
    MissingHoursReport,
    missingHoursReport,
    -- * Data
    MissingHour (..),
    -- * Logic
    missingHoursForUser,
    -- * Lenses
    missingHourDay,
    missingHourCapacity,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Fixed                (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns
import Futurice.Time

import qualified Data.Map          as Map
import qualified Data.Tuple.Strict as S
import qualified Data.Vector       as V
import qualified FUM
import qualified PlanMill          as PM
import qualified PlanMill.Queries  as PMQ

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

instance ToJSON MissingHour where toJSON = sopToJSON
instance FromJSON MissingHour where parseJSON = sopParseJSON
instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns MissingHour where
    type Columns MissingHour = '[Day, NDT 'Hours Centi]
    toColumns (MissingHour d c) = [I d :* I c :* Nil]

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type MissingHoursReport = Report
    "Missing hour markings"
    ReportGenerated
    (HashMap FUM.UserName :$ StrictPair Employee :$ Vector :$ MissingHour)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | TODO: use applicative
--
-- /TODO:/ should return Map Day DoubleMissingHour?
missingHoursForUser
    :: MonadPlanMillQuery m
    => PM.Interval Day
    -> PM.UserId
    -> m (Vector MissingHour)
missingHoursForUser interval uid = do
    tr <- PMQ.timereports interval uid
    uc <- PMQ.capacities interval uid
    pure $ mkMissingHours tr uc
  where
    mkMissingHours :: PM.Timereports -> PM.UserCapacities -> Vector MissingHour
    mkMissingHours tr uc
        = V.fromList
        . map (uncurry MissingHour)
        . Map.toList
        $ Map.differenceWith minus uc' tr'
      where
        tr' :: Map Day (NDT 'Hours Centi)
        tr' = Map.fromList
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
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => PM.Interval Day
    -> m MissingHoursReport
missingHoursReport interval = do
    now <- PM.currentTime
    fpm <- fumPlanmillMap
    fpm' <- traverse (perUser . view PM.identifier) fpm
    pure $ Report (ReportGenerated now) fpm'
  where
    perUser :: PM.UserId -> m (StrictPair Employee :$ Vector :$ MissingHour)
    perUser pmUid = (S.:!:)
        <$> planmillEmployee pmUid
        <*> missingHoursForUser interval pmUid
