{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Futurice.App.Spice.Types where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.TH
import Data.Char     (toLower)
import Servant.Docs  (ToSample (..))

import qualified Chat.Flowdock.REST as FD
import qualified GitHub             as GH

import Futurice.App.Spice.Orphans ()

data Contribution = Contribution
    { _contrHours   :: !Float
    , _contrAuthor  :: !FD.Author
    , _contrSubject :: !Text
    , _contrGithub  :: !(Maybe (GH.Name GH.Owner, GH.Name GH.Repo))
    }
    deriving (Show, Generic, Typeable)

makeLenses ''Contribution
deriveJSON defaultOptions{fieldLabelModifier = drop 6, constructorTagModifier = map toLower} ''Contribution

instance AnsiPretty Contribution

data SpiceRepo = SpiceRepo
    { _repoOwner       :: !(GH.Name GH.Owner)
    , _repoName        :: !(GH.Name GH.Repo)
    , _repoDescription :: !(Maybe Text)
    , _repoStarsCount  :: !Int
    , _repoLanguage    :: !(Maybe GH.Language)
    }
    deriving (Show, Generic, Typeable)

makeLenses ''SpiceRepo
deriveJSON defaultOptions{fieldLabelModifier = drop 5, constructorTagModifier = map toLower} ''SpiceRepo

instance AnsiPretty SpiceRepo

data Stats = Stats
    { _statsContributions :: !(Vector Contribution)
    , _statsContributors  :: !(Vector Text)
    , _statsGithubRepos   :: !(Vector SpiceRepo)
    }
    deriving (Show, Generic, Typeable)

makeLenses ''Stats
deriveJSON defaultOptions{fieldLabelModifier = drop 6, constructorTagModifier = map toLower} ''Stats

instance AnsiPretty Stats

-- | /TODO:/ write better sample
instance ToSample Stats where
    toSamples _ = [("empty stats", Stats mempty mempty mempty)]
