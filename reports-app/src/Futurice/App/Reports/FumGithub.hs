{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | FUM
module Futurice.App.Reports.FumGithub (
    -- * Report
    FumGitHubReport,
    fumGithubReport,
    -- * Types
    FumGithubReportParams (..),
    GitHubUser (..),
    FUMUser (..),
    -- * Lenses
    ghUserName, ghUserLogin,
    fumUserName, fumUserLogin, fumGhLogin,
    fumGithubReportGenerated, fumGithubReportFUMBaseUrl,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

import Control.Arrow             ((&&&))
import Data.Maybe                (mapMaybe)
import Futurice.Lucid.Foundation
import Futurice.Report.Columns
import Network.HTTP.Client       (Manager)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified FUM
import qualified GitHub              as GH

import Futurice.App.Reports.Config

--import Data.Time.Format.Human    (humanReadableTime')
-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data GitHubUser = GitHubUser
    { _ghUserName  :: !(Maybe Text)
    , _ghUserLogin :: !Text
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

data FUMUser = FUMUser
    { _fumUserName  :: !Text
    , _fumUserLogin :: !Text
    , _fumGhLogin   :: !Text
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''GitHubUser
makeLenses ''FUMUser

deriveGeneric ''GitHubUser
deriveGeneric ''FUMUser

instance NFData GitHubUser
instance NFData FUMUser

instance ToJSON GitHubUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToJSON FUMUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToSchema GitHubUser where
    declareNamedSchema = sopDeclareNamedSchema
instance ToSchema FUMUser where
    declareNamedSchema = sopDeclareNamedSchema


instance ToColumns GitHubUser
instance ToColumns FUMUser

{-
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
-}

data FumGithubReportParams = FumGithubReportParams
    { _fumGithubReportGenerated  :: !UTCTime
    , _fumGithubReportFUMBaseUrl :: !Text
    }
    deriving (Typeable, Generic)

makeLenses ''FumGithubReportParams
deriveGeneric ''FumGithubReportParams

instance NFData FumGithubReportParams

instance ToJSON FumGithubReportParams where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToSchema FumGithubReportParams where
    declareNamedSchema = sopDeclareNamedSchema

instance ToHtml FumGithubReportParams where
    toHtml (FumGithubReportParams r _) = toHtml $ show r
    toHtmlRaw = toHtml


-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type FumGitHubReport = Report
    "Users in FUM ↔ GitHub"
    FumGithubReportParams
    (Vector (These GitHubUser FUMUser))

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

{-
-- | See <http://stackoverflow.com/questions/5890094/is-there-a-way-to-define-an-existentially-quantified-newtype-in-ghc-haskell>
data E envC m where
    MkE :: Dict (MonadReader env m, envC env) -> E envC m

class MonadReader' envC m where
    monadReaderUnwrap :: E envC m

instance (MonadReader env m, c env) => MonadReader' c m where
    monadReaderUnwrap = MkE Dict
-}

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

fumGithubReport
    :: Manager
    -> Config
    -> IO FumGitHubReport
fumGithubReport mgr cfg = do
    now <- currentTime
    fs <- fumUsers
    gs <- githubUsers
    return $ Report (FumGithubReportParams now $ cfgFumPubUrl cfg) $ makeReport gs fs
  where
    fumUsers :: IO (Vector FUMUser)
    fumUsers = V.fromList . mapMaybe mk . V.toList <$>
        FUM.fetchList mgr (cfgFumAuth cfg) (cfgFumBaseUrl cfg) (cfgFumUserList cfg)
      where
        mk u = case u ^. FUM.userGithub ^. lazy of
            Nothing -> Nothing
            Just ghLogin -> Just $ FUMUser
                { _fumUserName  = u ^. FUM.userFirst <> " " <> u ^. FUM.userLast
                , _fumUserLogin = u ^. FUM.userName ^. FUM.getUserName
                , _fumGhLogin   = ghLogin
                }

    githubUsers :: IO (Vector GitHubUser)
    githubUsers = do
        let team = cfgGhTeam cfg
        teams <- exec $ GH.teamsOfR (cfgGhOrg cfg) GH.FetchAll
        case V.find ((team ==) . GH.simpleTeamName) teams of
            Nothing ->
                traverse mk =<< exec (GH.membersOfR (cfgGhOrg cfg) GH.FetchAll)
            Just t ->
                traverse mk =<< exec (GH.listTeamMembersR (GH.simpleTeamId t) GH.TeamMemberRoleAll GH.FetchAll)

      where
        exec :: GH.Request k a -> IO a
        exec req = do
            print req
            r <- GH.executeRequestWithMgr mgr (cfgGhAuth cfg) req
            either throwM pure r

        mk u = do
            u' <- exec (GH.userInfoForR (GH.simpleUserLogin u))
            pure $ GitHubUser
                { _ghUserName  = GH.userName u'
                , _ghUserLogin = GH.untagName $ GH.userLogin u'
                }

    -- | Align on `
    makeReport
        :: Vector GitHubUser -> Vector FUMUser
        -> Vector (These GitHubUser FUMUser)
    makeReport gs fs =
        -- Create 'HashMap FUMLogin a' maps, and 'align' them
        let gs' = HM.fromList . map (_ghUserLogin &&& id) . V.toList $ gs
            fs' = HM.fromList . map (_fumGhLogin &&& id) . V.toList $ fs
            hm  = align gs' fs'
        in V.fromList . sort . HM.elems $ hm
