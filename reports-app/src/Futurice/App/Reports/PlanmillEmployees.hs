{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Planmill Employees
module Futurice.App.Reports.PlanmillEmployees (
    -- * Report
    PlanmillEmployeesReport,
    planmillEmployeesReport,
    -- * Types
    PlanmillEmployee (..),
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

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PlanmillEmployee = PlanmillEmployee
    { _pmeFirstName    :: !Text
    , _pmeLastName     :: !Text
    , _pmeTitle        :: !Text
    , _pmeSuperior     :: !Text
    , _pmeTeam         :: !Text
    , _pmeContract     :: !Text
    , _pmePhone        :: !Text
    , _pmeEmail        :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-- makeLenses ''PlanmillEmployee
deriveGeneric ''PlanmillEmployee

instance NFData PlanmillEmployee
instance ToJSON PlanmillEmployee where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON PlanmillEmployee where parseJSON = sopParseJSON
instance ToSchema PlanmillEmployee where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns PlanmillEmployee where
    columnNames _ =
        K "first-name" :*
        K "last-name" :*
        K "title" :*
        K "superior" :*
        K "team" :*
        K "contract" :*
        K "phone" :*
        K "email" :*
        Nil

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type PlanmillEmployeesReport = Report
    "Employees in PlanMill"
    ReportGenerated
    (Vector PlanmillEmployee) 

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

planmillEmployeesReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m PlanmillEmployeesReport
planmillEmployeesReport = do
    now <- currentTime
    pure $ Report (ReportGenerated now) mempty
