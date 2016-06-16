{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports.Types where

import Futurice.Prelude

import Data.Aeson                (ToJSON (..))
import Data.Swagger              (ToSchema (..))
import Data.These                (These (..))
import Data.Time.Format.Human    (humanReadableTime')
import Futurice.Generics         (sopDeclareNamedSchema, sopToJSON)
import Futurice.Peano            (PThree, PTwo)
import Futurice.Report
import Lucid
import Lucid.Foundation.Futurice

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
            li_ $ a_ [href_ "/fum-github" ] $ "Users in FUM and GitHub"

-------------------------------------------------------------------------------
-- Issues
-------------------------------------------------------------------------------

data GitHubRepo = GitHubRepo
    { ghRepoOwner :: !(GH.Name GH.Owner)
    , ghRepoName  :: !(GH.Name GH.Repo)
    }
    deriving (Typeable)

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

    type ReportRowC IssueInfo m = MonadReader ReportGenerated m

    reportHeader _ = ReportHeader
        $ IList.cons "#"
        $ IList.cons "title"
        $ IList.cons "created"
        $ IList.nil

    reportRow (IssueInfo n t c u) = [ReportRow mempty row]
      where
        c' = do
            ReportGenerated now <- ask
            return $ humanReadableTime' now c

        row = IList.cons (a_ [href_ u] $ toHtml $ "#" ++ show n)
            $ IList.cons (a_ [href_ u] $ toHtml t)
            $ IList.cons (c' >>= toHtml)
            $ IList.nil

makeLenses ''IssueInfo
deriveGeneric ''IssueInfo
instance ToJSON IssueInfo where toJSON = sopToJSON
instance ToSchema IssueInfo where declareNamedSchema = sopDeclareNamedSchema


type IssueReport = Report
    "GitHub issues"
    ReportGenerated
    (Vector :$ Per GitHubRepo :$ Vector :$ IssueInfo)

instance IsReport ReportGenerated (Vector :$ Per GitHubRepo :$ Vector :$ IssueInfo) where
    reportExec = readerReportExec

-------------------------------------------------------------------------------
-- Github + FUM
-------------------------------------------------------------------------------

data GitHubUser = GitHubUser
    { ghUserName  :: !(Maybe Text)
    , ghUserLogin :: !Text
    }
    deriving (Eq, Ord, Show, Typeable)

data FUMUser = FUMUser
    { fumUserName  :: !Text
    , fumUserLogin :: !Text
    , fumGhLogin   :: !Text
    }
    deriving (Eq, Ord, Show, Typeable)

deriveGeneric ''GitHubUser
deriveGeneric ''FUMUser

instance ToJSON GitHubUser where toJSON = sopToJSON
instance ToJSON FUMUser where toJSON = sopToJSON

instance ToReportRow GitHubUser where
    type ReportRowLen GitHubUser = PTwo

    reportHeader _ = ReportHeader
        $ IList.cons "GH name"
        $ IList.cons "GH login"
        $ IList.nil

    reportRow (GitHubUser n l) = [ReportRow mempty row]
      where
        row = IList.cons (maybe (em_ "???") toHtml n)
            $ IList.cons (a_ [href_ $ "https://github.com/" <> l] $ toHtml l)
            $ IList.nil

instance ToReportRow FUMUser where
    type ReportRowLen FUMUser = PThree

    reportHeader _ = ReportHeader
        $ IList.cons "FUM name"
        $ IList.cons "FUM login"
        $ IList.cons "FUM GH login"
        $ IList.nil

    reportRow (FUMUser n l g) = [ReportRow mempty row]
      where
        row = IList.cons (toHtml n)
            $ IList.cons (toHtml l)
            $ IList.cons (a_ [href_ $ "https://github.com/" <> g] $ toHtml g)
            $ IList.nil

type FumGitHubReport = Report
    "Users in FUM <-> GitHub"
    ReportGenerated
    (Vector (These GitHubUser FUMUser))

instance IsReport ReportGenerated (Vector (These GitHubUser FUMUser)) where
    reportExec = defaultReportExec
