{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.Integrations.Types where

import Futurice.Prelude
import Prelude ()

import qualified Data.Csv          as Csv
import           Futurice.Generics
import qualified Futurice.IC       as IList
import           Futurice.Peano
import           Futurice.Report
import           Lucid             hiding (for_)

-- | Employee information often used in reports
data Employee = Employee
    { employeeName     :: !Text
    , employeeTeam     :: !Text
    , employeeContract :: !Text
    }

deriveGeneric ''Employee

instance ToReportRow Employee where
    type ReportRowLen Employee = PThree

    reportHeader _ = ReportHeader
        $ IList.cons "name"
        $ IList.cons "team"
        $ IList.cons "contract"
        $ IList.nil

    reportRow (Employee n t c) = [r]
      where
        r = ReportRow mempty
            $ IList.cons (toHtml n)
            $ IList.cons (toHtml t)
            $ IList.cons (toHtml c)
            $ IList.nil

    reportCsvRow (Employee n t c) = [r]
      where
        r = ReportCsvRow
            $ IList.cons (pure $ Csv.toField n)
            $ IList.cons (pure $ Csv.toField t)
            $ IList.cons (pure $ Csv.toField c)
            $ IList.nil

instance ToJSON Employee where toJSON = sopToJSON
instance FromJSON Employee where parseJSON = sopParseJSON
