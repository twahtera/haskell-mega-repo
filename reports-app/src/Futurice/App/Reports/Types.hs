{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.Reports.Types where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson                (ToJSON (..))
import Data.Swagger              (ToSchema (..))
import Futurice.Generics         (sopDeclareNamedSchema, sopToJSON)
import Futurice.Peano (PThree, PTwo)
import Futurice.Report
import Lucid
import Lucid.Foundation.Futurice

-- import Data.Time.Format.Human    (humanReadableTime')

import qualified Futurice.IC as IList
import qualified GitHub      as GH


-------------------------------------------------------------------------------
-- Indexpage
-------------------------------------------------------------------------------

data IndexPage = IndexPage
    
instance ToHtml IndexPage where
    toHtmlRaw = toHtml
    toHtml _ = page_ "Reports" $ do
        row_ $ large_ 12 $ h1_ "Reports"
        row_ $ large_ 12 $ div_ [class_ "callout"] $ ul_ $ do
            li_ $ a_ [href_ "/issues" ] $ "GitHub issues"

-------------------------------------------------------------------------------
-- Issues
-------------------------------------------------------------------------------

data GitHubRepo = GitHubRepo
    { ghRepoOwner :: !(GH.Name GH.Owner)
    , ghRepoName  :: !(GH.Name GH.Repo)
    }

instance ToReportRow GitHubRepo where
    type ReportRowLen GitHubRepo = PTwo

    reportHeader _ = ReportHeader
        $ IList.cons "owner"
        $ IList.cons "repository"
        $ IList.nil

    reportRow (GitHubRepo o n) = [ReportRow mempty row]
      where
        row = IList.cons (a_ [href_ ownerLink] $ toHtml o')
            $ IList.cons (a_ [href_ repoLink] $ toHtml n')
            $ IList.nil

        o' = GH.untagName o
        n' = GH.untagName n
        ownerLink = "https://github.com/" <> o'
        repoLink = ownerLink <> "/" <> n'

deriveGeneric ''GitHubRepo
instance ToJSON GitHubRepo where toJSON = sopToJSON


data IssueInfo = IssueInfo
    { _issueNumber  :: !Int
    , _issueTitle   :: !Text
    , _issueCreated :: !UTCTime
    , _issueUrl     :: !Text
    }
    deriving (Show, Generic, Typeable)

instance ToReportRow IssueInfo where
    type ReportRowLen IssueInfo = PThree

    reportHeader _ = ReportHeader
        $ IList.cons "#"
        $ IList.cons "title"
        $ IList.cons "created"
        $ IList.nil

    reportRow (IssueInfo n t c u) = [ReportRow mempty row]
      where
        row = IList.cons (a_ [href_ u] $ toHtml $ "#" ++ show n)
            $ IList.cons (a_ [href_ u] $ toHtml t)
            $ IList.cons (toHtml $ show c)
            $ IList.nil

makeLenses ''IssueInfo
deriveGeneric ''IssueInfo
instance ToJSON IssueInfo where toJSON = sopToJSON
instance ToSchema IssueInfo where declareNamedSchema = sopDeclareNamedSchema


type IssueReport = Report
    "GitHub issues"
    ReportGenerated
    '[Vector, Per GitHubRepo, Vector]
    IssueInfo
