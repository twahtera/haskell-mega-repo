{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.Balances (
    -- * Report
    BalanceReport,
    balanceReport,
    -- * Types
    Balance (..),
    BalanceKind (..),
    -- * Logic
    balanceForUser,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (hasn't, sumOf, to)
import Data.Fixed                (Centi)
import Data.Ord                  (comparing)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Peano
import Futurice.Report
import Futurice.Time

import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import qualified Data.Vector         as V
import qualified Futurice.IC         as IList
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

import Futurice.App.Reports.MissingHours
       (missingHourCapacity, missingHoursForUser)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data BalanceKind = BalanceUnder | BalanceNormal | BalanceOver

makePrisms ''BalanceKind

instance ToHtml BalanceKind where
    toHtmlRaw = toHtml
    toHtml BalanceUnder  = b_ "under"
    toHtml BalanceNormal = "ok"
    toHtml BalanceOver   = b_ "over"

instance Csv.ToField BalanceKind where
    toField BalanceUnder  = "under"
    toField BalanceNormal = "ok"
    toField BalanceOver   = "over"

balanceKind :: (Num a, Ord a) => NDT 'Hours a -> BalanceKind
balanceKind h
    | h < (-20) = BalanceUnder
    | h > 40    = BalanceOver
    | otherwise = BalanceNormal
{-# INLINE balanceKind #-}

data Balance = Balance
    { balanceHours        :: !(NDT 'Hours Centi)
    , balanceMissingHours :: !(NDT 'Hours Centi)
    }
    deriving (Eq, Ord, Show, Typeable, Generic)


instance ToReportRow Balance where
    type ReportRowLen Balance = PFour

    reportHeader _ = ReportHeader
        $ IList.cons "hours"
        $ IList.cons "missing"
        $ IList.cons "difference"
        $ IList.cons "kind"
        $ IList.nil

    reportRow (Balance hours missing) = [r]
      where
        diff = hours + missing
        cls | hasn't (to balanceKind . _BalanceNormal) diff  = "emphasize"
            | hasn't (to balanceKind . _BalanceNormal) hours = "emphasize2"
            | otherwise                                     = "normal"

        r = ReportRow (Set.singleton cls)
            $ IList.cons (toHtml hours)
            $ IList.cons (toHtml missing)
            $ IList.cons (toHtml diff)
            $ IList.cons (toHtml $ balanceKind diff )
            $ IList.nil

    reportCsvRow (Balance hours missing) = [r]
      where
        diff = hours + missing
        r = ReportCsvRow
            $ IList.cons (pure $ Csv.toField hours)
            $ IList.cons (pure $ Csv.toField missing)
            $ IList.cons (pure $ Csv.toField diff)
            $ IList.cons (pure $ Csv.toField $ balanceKind diff)
            $ IList.nil

deriveGeneric ''Balance

instance NFData Balance
instance ToJSON Balance where toJSON = sopToJSON
instance FromJSON Balance where parseJSON = sopParseJSON
instance ToSchema Balance where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type BalanceReport = Report
    "Hour marking flex saldos"
    ReportGenerated
    (Vector :$ Per Employee :$ Balance)

instance IsReport
    ReportGenerated
    (Vector :$ Per Employee :$ Balance)
  where
    reportExec = defaultReportExec

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

balanceForUser
    :: MonadPlanMillQuery m
    => PM.Interval Day
    -> PM.UserId
    -> m Balance
balanceForUser interval uid = do
    PM.TimeBalance balanceMinutes <- PMQ.userTimebalance uid
    let balanceMinutes' = ndtConvert' balanceMinutes
    mh <- missingHoursForUser interval uid
    pure $ Balance
        { balanceHours        = balanceMinutes'
        , balanceMissingHours = sumOf (folded . missingHourCapacity) mh
        }

balanceReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => PM.Interval Day
    -> m BalanceReport
balanceReport interval = do
    now <- currentTime
    fpm <- fumPlanmillMap
    fpm' <- traverse (perUser . view PM.identifier) fpm
    let fpm'' = V.fromList . sortBy cmpPE . HM.elems $ fpm'
    pure $ Report (ReportGenerated now) fpm''
  where
    cmpPE :: Per Employee a -> Per Employee a -> Ordering
    cmpPE = (comparing employeeTeam <> comparing employeeName) `on` perFst

    -- TODO: put planmillEmployee into fumPlanmillMap!
    -- Also MissingHours report
    perUser :: PM.UserId -> m (Per Employee Balance)
    perUser pmUid = Per
        <$> planmillEmployee pmUid
        <*> balanceForUser interval pmUid
