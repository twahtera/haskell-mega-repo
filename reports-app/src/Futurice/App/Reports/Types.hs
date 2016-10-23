{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
---
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.Reports.Types where

import Futurice.Prelude
import Prelude ()

import Data.Aeson                (ToJSON (..))
import Data.Constraint           (Dict (..))
import Data.Swagger              (ToSchema (..))
import Data.Time.Format.Human    (humanReadableTime')
import Futurice.Generics         (sopDeclareNamedSchema, sopToJSON)
import Futurice.Peano            (PThree, PTwo)
import Futurice.Report
import Futurice.Lucid.Foundation

import qualified Data.Csv    as Csv
import qualified Futurice.IC as IList
import qualified GitHub      as GH

-------------------------------------------------------------------------------
-- Issues
-------------------------------------------------------------------------------

data GitHubRepo = GitHubRepo
    { ghRepoOwner :: !(GH.Name GH.Owner)
    , ghRepoName  :: !(GH.Name GH.Repo)
    }
    deriving (Typeable, Generic)

instance NFData GitHubRepo

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

    reportCsvRow (GitHubRepo o n) = [ReportCsvRow row]
      where
        row = IList.cons (pure $ Csv.toField o')
            $ IList.cons (pure $ Csv.toField n')
            $ IList.nil

        o' = GH.untagName o
        n' = GH.untagName n

deriveGeneric ''GitHubRepo
instance ToJSON GitHubRepo where toJSON = sopToJSON


data IssueInfo = IssueInfo
    { _issueNumber  :: !Int
    , _issueTitle   :: !Text
    , _issueCreated :: !UTCTime
    , _issueUrl     :: !Text
    }
    deriving (Show, Generic, Typeable)

instance NFData IssueInfo

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

    reportCsvRow (IssueInfo n t c _) = [ReportCsvRow row]
      where
        row = IList.cons (pure $ Csv.toField n)
            $ IList.cons (pure $ Csv.toField t)
            $ IList.cons (pure $ Csv.toField $ show c)
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
    deriving (Eq, Ord, Show, Typeable, Generic)

data FUMUser = FUMUser
    { fumUserName  :: !Text
    , fumUserLogin :: !Text
    , fumGhLogin   :: !Text
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''GitHubUser
deriveGeneric ''FUMUser

instance NFData GitHubUser
instance NFData FUMUser

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

    reportCsvRow (GitHubUser n l) = [ReportCsvRow row]
      where
        row = IList.cons (pure $ Csv.toField n)
            $ IList.cons (pure $ Csv.toField l)
            $ IList.nil

instance ToReportRow FUMUser where
    type ReportRowLen FUMUser = PThree
    type ReportRowC FUMUser m = MonadReader' HasFUMPublicURL m

    reportHeader _ = ReportHeader
        $ IList.cons "FUM name"
        $ IList.cons "FUM login"
        $ IList.cons "FUM GH login"
        $ IList.nil

    reportRow
        :: forall m. (Monad m, ReportRowC FUMUser m)
        => FUMUser -> [ReportRow m (ReportRowLen FUMUser)]
    reportRow (FUMUser n l g) = case monadReaderUnwrap :: E HasFUMPublicURL m of
        MkE Dict -> let
            l' = do
                u <- lift (view fumPublicUrl)
                a_ [href_ $ u <> "/fum/users/" <> l <> "/"] $ toHtml l
            row = IList.cons (toHtml n)
                $ IList.cons l'
                $ IList.cons (a_ [href_ $ "https://github.com/" <> g] $ toHtml g)
                $ IList.nil
            in [ReportRow mempty row]

    reportCsvRow (FUMUser n l g) = [ReportCsvRow row]
      where
        row = IList.cons (pure $ Csv.toField n)
            $ IList.cons (pure $ Csv.toField l)
            $ IList.cons (pure $ Csv.toField g)
            $ IList.nil


data FumGithubReportParams = FumGithubReportParams !UTCTime !Text
    deriving (Typeable, Generic)

instance NFData FumGithubReportParams

instance ToHtml FumGithubReportParams where
    toHtml (FumGithubReportParams r _) = toHtml $ show r
    toHtmlRaw = toHtml

instance ToJSON FumGithubReportParams where
    toJSON (FumGithubReportParams t _) = toJSON t

type FumGitHubReport = Report
    "Users in FUM ↔ GitHub"
    FumGithubReportParams
    (Vector (These GitHubUser FUMUser))

instance IsReport FumGithubReportParams (Vector (These GitHubUser FUMUser)) where
    reportExec = readerReportExec

-- | *TODO:* move to @futurice-integrations@ package

class HasFUMPublicURL env where
    fumPublicUrl :: Lens' env Text

instance HasFUMPublicURL FumGithubReportParams where
    fumPublicUrl = lens t f
      where
        t (FumGithubReportParams _ x) = x
        f (FumGithubReportParams y _) = FumGithubReportParams y

-------------------------------------------------------------------------------
-- This might be a bad idea?
-------------------------------------------------------------------------------

-- | See <http://stackoverflow.com/questions/5890094/is-there-a-way-to-define-an-existentially-quantified-newtype-in-ghc-haskell>
data E envC m where
    MkE :: Dict (MonadReader env m, envC env) -> E envC m

class MonadReader' envC m where
    monadReaderUnwrap :: E envC m

instance (MonadReader env m, c env) => MonadReader' c m where
    monadReaderUnwrap = MkE Dict
