{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.GitHubDashboard.Types where

import Futurice.Prelude
import Prelude          ()

import Control.Lens (itraverse_)
import Data.Aeson        (ToJSON (..))
import Data.Swagger      (ToSchema (..))
import Futurice.Generics (sopDeclareNamedSchema, sopToJSON)
import Servant.Docs      (ToSample (..))
import Lucid
import Lucid.Foundation.Futurice
import Data.Time.Format.Human (humanReadableTime')

import qualified Data.Map.Strict as Map

import qualified GitHub as GH

import Futurice.App.GitHubDashboard.Orphans ()

-------------------------------------------------------------------------------
-- Pull requests
-------------------------------------------------------------------------------

data PullRequestInfo = PullRequestInfo
    { _prNumber  :: !Int
    , _prTitle   :: !Text
    , _prCreated :: !UTCTime
    , _prUrl     :: !Text
    }
    deriving (Show, Generic, Typeable)

makeLenses ''PullRequestInfo
deriveGeneric ''PullRequestInfo
instance ToJSON PullRequestInfo where toJSON = sopToJSON
instance ToSchema PullRequestInfo where declareNamedSchema = sopDeclareNamedSchema

newtype PullRequests = PullRequests (Map (GH.Name GH.Owner, GH.Name GH.Repo) (Vector PullRequestInfo))
    deriving (Show, Generic, Typeable)

makeLenses ''PullRequests
instance ToJSON PullRequests where
    toJSON (PullRequests prs) = toJSON $ Map.mapKeys f prs
      where
        f (owner, repo) = GH.untagName owner <> "/" <> GH.untagName repo

instance ToSchema PullRequests where
    declareNamedSchema _ =
        declareNamedSchema (Proxy :: Proxy (Map Text (Vector PullRequestInfo)))

instance ToSample PullRequests where
    toSamples _ = [("empty stats", PullRequests mempty)]

-------------------------------------------------------------------------------
-- Issues
-------------------------------------------------------------------------------

data IssueInfo = IssueInfo
    { _issueNumber  :: !Int
    , _issueTitle   :: !Text
    , _issueCreated :: !UTCTime
    , _issueUrl     :: !Text
    }
    deriving (Show, Generic, Typeable)

makeLenses ''IssueInfo
deriveGeneric ''IssueInfo
instance ToJSON IssueInfo where toJSON = sopToJSON
instance ToSchema IssueInfo where declareNamedSchema = sopDeclareNamedSchema

data Issues = Issues
    { issuesNow  :: UTCTime
    , issuesData :: Map (GH.Name GH.Owner, GH.Name GH.Repo) (Vector IssueInfo)
    } 
    deriving (Show, Generic, Typeable)

makeLenses ''Issues

instance ToJSON Issues where
    toJSON (Issues _ prs) = toJSON $ Map.mapKeys f prs
      where
        f (owner, repo) = GH.untagName owner <> "/" <> GH.untagName repo

-- | TODO: fixme
instance ToSchema Issues where
    declareNamedSchema _ =
        declareNamedSchema (Proxy :: Proxy (Map Text (Vector IssueInfo)))

instance ToSample Issues where
    toSamples _ = [("empty stats", Issues undefined mempty)]

instance ToHtml Issues where
    toHtmlRaw _ = pure ()
    toHtml (Issues now issues) = page_ "Github issues" $ do
        h1_ "GitHub issues"
        span_ $ toHtml $ "Updated at " <> show now
        table_ $ do
            tr_ $ do
                th_ "repo"
                th_ "issue / pr"
                th_ "created"
            flip itraverse_ issues $ \ownerrepo -> traverse_ $ \info ->
                tr_ $ do
                    td_ $ repoLink_ ownerrepo
                    td_ $ a_ [href_ $ info ^. issueUrl ] $ do
                        "#"
                        toHtml $ show $ info ^. issueNumber
                        ": "
                        toHtml $ info ^. issueTitle
                    td_ $ toHtml $ humanReadableTime' now $ info ^. issueCreated

repoLink_ :: Monad m => (GH.Name a, GH.Name b) -> HtmlT m ()
repoLink_ (owner, repo) =
    let x =  GH.untagName owner <> "/" <> GH.untagName repo
    in a_ [href_ $ "https://github.com/" <> x ] $ toHtml x
