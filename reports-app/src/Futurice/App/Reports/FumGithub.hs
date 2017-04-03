{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
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
    fumGithubReportGenerated, fumGithubReportFumPublicUrl,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

import Control.Arrow             ((&&&))
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Report.Columns

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified FUM
import qualified GitHub              as GH

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
    , _fumUserLogin :: !FUM.UserName
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

data FumGithubReportParams = FumGithubReportParams
    { _fumGithubReportGenerated    :: !UTCTime
    , _fumGithubReportFumPublicUrl :: !Text
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
    (Vector TheseGhFumUser)

newtype TheseGhFumUser = TheseGhFumUser (These GitHubUser FUMUser)
  deriving (Eq, Ord, Show, Generic)

instance NFData TheseGhFumUser
instance ToJSON TheseGhFumUser
instance ToSchema TheseGhFumUser

instance ToColumns TheseGhFumUser where
    type Columns TheseGhFumUser = Text ': Columns (These GitHubUser FUMUser)

    columnNames _ =
        K "extra" :*
        columnNames (Proxy :: Proxy (These GitHubUser FUMUser))

    toColumns (TheseGhFumUser u) = (I extra :*) <$> toColumns u
      where
        extra = case u of
            These gh fum | gh ^. ghUserLogin /= fum ^. fumGhLogin ->
                "Case mismatch"
            _ -> ""

instance HasFUMPublicURL FumGithubReportParams where
    fumPublicUrl = fumGithubReportFumPublicUrl

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

fumGithubReport
    :: forall m env.
       ( MonadTime m, MonadFUM m, MonadGitHub m
       , MonadReader env m
       , HasGithubOrgName env, HasFUMEmployeeListName env, HasFUMPublicURL env
       )
    => m FumGitHubReport
fumGithubReport = do
    now <- currentTime
    fumPubUrl <- view fumPublicUrl
    report <- makeReport <$> githubUsers <*> fumUsers
    return $ Report (FumGithubReportParams now fumPubUrl) report
  where
    fumUsers :: m (Vector FUMUser)
    fumUsers = V.fromList . mapMaybe mk . V.toList <$> fumEmployeeList
      where
        mk u = case u ^. FUM.userGithub ^. lazy of
            Nothing -> Nothing
            Just ghLogin -> Just $ FUMUser
                { _fumUserName  = u ^. FUM.userFirst <> " " <> u ^. FUM.userLast
                , _fumUserLogin = u ^. FUM.userName
                , _fumGhLogin   = ghLogin
                }

    githubUsers :: m (Vector GitHubUser)
    githubUsers = mk <$$> githubDetailedMembers
      where
        mk u = GitHubUser
            { _ghUserName  = GH.userName u
            , _ghUserLogin = GH.untagName $ GH.userLogin u
            }

    -- | Align on `
    makeReport
        :: Vector GitHubUser -> Vector FUMUser
        -> Vector TheseGhFumUser
    makeReport gs fs =
        -- Create 'HashMap FUMLogin a' maps, and 'align' them
        let gs' = HM.fromList . map (T.toLower . _ghUserLogin &&& id) . V.toList $ gs
            fs' = HM.fromList . map (T.toLower . _fumGhLogin &&& id) . V.toList $ fs
            hm  = alignWith TheseGhFumUser gs' fs'
        in V.fromList . sort . HM.elems $ hm

githubDetailedMembers
    :: ( MonadGitHub m
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.User)
githubDetailedMembers = do
    githubMembers <- githubOrganisationMembers
    traverse (githubReq . GH.userInfoForR . GH.simpleUserLogin) githubMembers
