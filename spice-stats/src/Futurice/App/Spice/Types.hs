{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.Spice.Types where

import           Futurice.Generics
import           Futurice.Prelude
import qualified GitHub            as GH
import           Prelude ()

data Author = Author
    { _authorName   :: !(Maybe Text)
    , _authorEmail  :: !(Maybe Text)
    , _authorAvatar :: !(Maybe Text)
    }
    deriving (Show, Generic, Typeable)

makeLenses ''Author
deriveGeneric ''Author
instance AnsiPretty Author
instance ToJSON Author where toJSON = sopToJSON
instance ToSchema Author where declareNamedSchema = sopDeclareNamedSchema

data Contribution = Contribution
    { _contrHours   :: !Float
    , _contrAuthor  :: !Author
    , _contrSubject :: !Text
    , _contrGithub  :: !(Maybe (GH.Name GH.Owner, GH.Name GH.Repo))
    }
    deriving (Show, Generic, Typeable)

makeLenses ''Contribution
deriveGeneric ''Contribution
instance AnsiPretty Contribution
instance ToJSON Contribution where toJSON = sopToJSON
instance ToSchema Contribution where declareNamedSchema = sopDeclareNamedSchema

data SpiceRepo = SpiceRepo
    { _repoOwner       :: !(GH.Name GH.Owner)
    , _repoName        :: !(GH.Name GH.Repo)
    , _repoDescription :: !(Maybe Text)
    , _repoStarsCount  :: !Int
    , _repoLanguage    :: !(Maybe GH.Language)
    }
    deriving (Show, Generic, Typeable)

makeLenses ''SpiceRepo
deriveGeneric ''SpiceRepo
instance AnsiPretty SpiceRepo
instance ToJSON SpiceRepo where toJSON = sopToJSON
instance ToSchema SpiceRepo where declareNamedSchema = sopDeclareNamedSchema

data Stats = Stats
    { _statsContributions :: !(Vector Contribution)
    , _statsContributors  :: !(Vector Text)
    , _statsGithubRepos   :: !(Vector SpiceRepo)
    }
    deriving (Show, Generic, Typeable)

makeLenses ''Stats
deriveGeneric ''Stats
instance AnsiPretty Stats
instance ToJSON Stats where toJSON = sopToJSON
instance ToSchema Stats where declareNamedSchema = sopDeclareNamedSchema
