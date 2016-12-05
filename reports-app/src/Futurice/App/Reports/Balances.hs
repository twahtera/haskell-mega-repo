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
    balanceKind,
    balanceForUser,
    -- * Lenses
    balanceHours, balanceMissingHours,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (sumOf)
import Data.Fixed                (Centi)
import Data.Ord                  (comparing)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Report.Columns
import Futurice.Time

import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Tuple.Strict   as S
import qualified Data.Vector         as V
import qualified FUM                 as FUM
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

import Futurice.App.Reports.MissingHours
       (missingHourCapacity, missingHoursForUser)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data BalanceKind = BalanceUnder | BalanceNormal | BalanceOver
  deriving (Eq, Ord)

makePrisms ''BalanceKind

instance ToJSON BalanceKind where
    toJSON BalanceUnder  = "under"
    toJSON BalanceNormal = "ok"
    toJSON BalanceOver   = "over"

instance ToHtml BalanceKind where
    toHtmlRaw = toHtml
    toHtml BalanceUnder  = b_ "under"
    toHtml BalanceNormal = "ok"
    toHtml BalanceOver   = b_ "over"

instance Csv.ToField BalanceKind where
    toField BalanceUnder  = "under"
    toField BalanceNormal = "ok"
    toField BalanceOver   = "over"

instance ReportValue BalanceKind

balanceKind :: (Num a, Ord a) => NDT 'Hours a -> BalanceKind
balanceKind h
    | h < (-20) = BalanceUnder
    | h > 40    = BalanceOver
    | otherwise = BalanceNormal
{-# INLINE balanceKind #-}

data Balance = Balance
    { _balanceHours        :: !(NDT 'Hours Centi)
    , _balanceMissingHours :: !(NDT 'Hours Centi)
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance ToColumns Balance where
    type Columns Balance =
        '[ NDT 'Hours Centi , NDT 'Hours Centi, NDT 'Hours Centi, BalanceKind ]

    columnNames _ = K "flex" :* K "missing" :* K "balance" :* K "kind" :* Nil
    toColumns (Balance f m) =
        [ I f :* I m :* I diff :* I (balanceKind diff) :* Nil ]
      where
        diff = f + m -- if you mark missing, your flex will increase!

makeLenses ''Balance
deriveGeneric ''Balance

instance NFData Balance
instance ToJSON Balance where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Balance where parseJSON = sopParseJSON
instance ToSchema Balance where declareNamedSchema = sopDeclareNamedSchema


newtype Supervisor = Supervisor Text
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''Supervisor

instance ToColumns Supervisor where
    columnNames _ = K "supervisor" :* Nil

instance NFData Supervisor
instance ToJSON Supervisor where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Supervisor where parseJSON = sopParseJSON
instance ToSchema Supervisor where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type BalanceReport = Report
    "Hour marking flex saldos"
    ReportGenerated
    (Vector :$ StrictPair Employee :$ StrictPair Supervisor :$ Balance)

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
        { _balanceHours        = balanceMinutes'
        , _balanceMissingHours = sumOf (folded . missingHourCapacity) mh
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
    fumUsers <- fumEmployeeList
    fpm <- fumPlanmillMap
    fpm' <- itraverse (perUser $ supervisors fumUsers) fpm
    let fpm'' = V.fromList . sortBy cmpPE . HM.elems $ fpm'
    pure $ Report (ReportGenerated now) fpm''
  where
    cmpPE :: StrictPair Employee a -> StrictPair Employee a -> Ordering
    cmpPE = (comparing employeeTeam <> comparing employeeName) `on` S.fst

    supervisors :: Vector FUM.User -> HashMap FUM.UserName Text
    supervisors us = HM.fromList . mapMaybe mk $ us'
      where
        us' = toList us

        mk u = (,) (u ^. FUM.userName) <$>
            (join $ flip HM.lookup ss <$> u ^. FUM.userSupervisor . lazy)

        -- id to name map
        ss :: HashMap Int Text
        ss = HM.fromList
            . map (\u -> (u ^. FUM.userId, u ^. FUM.userFullName))
            $ us'

    -- TODO: put planmillEmployee into fumPlanmillMap!
    -- Also MissingHours report
    perUser
        :: HashMap FUM.UserName Text
        -> FUM.UserName
        -> PM.User
        -> m (StrictPair Employee (StrictPair Supervisor Balance))
    perUser svs fumLogin pmUser = mk
        <$> planmillEmployee pmUid
        <*> pure (Supervisor $ fromMaybe "<unknown>" $ HM.lookup fumLogin svs)
        <*> balanceForUser interval pmUid
      where
        pmUid = pmUser ^. PM.identifier
        mk e s b = e S.:!: (s S.:!: b)
