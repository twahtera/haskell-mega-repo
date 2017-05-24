{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Smileys.Types (
    SmileysReport,
    PostSmiley(..),
    Res(..)
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.Report            (Report, ReportGenerated)
import Futurice.Report.Columns    (ToColumns, toColumns, Columns)

-------------------------------------------------------------------------------
-- Smiley
-------------------------------------------------------------------------------

data PostSmiley = PostSmiley
  { _postSmileyEntries :: ![Smiley]
  , _postSmileyDate :: !Day
  , _postSmileySmiley   :: !SmileyState
  } deriving (Eq, Ord, Show, Typeable, Generic)

data Res = Res
    { _resStatus :: !Text
    } deriving (Show)

type ProjectId = Int
type TaskId = Int
type SmileyState = Int

-- TODO: Naming: Smiley? SmileyEntries?
data Smiley = Smiley
  { _smileyProject :: !ProjectId
  , _smileyTask    :: !TaskId
  } deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Smiley
deriveGeneric ''Smiley

instance NFData Smiley
instance ToJSON Smiley where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Smiley where parseJSON = sopParseJSON
instance ToSchema Smiley where declareNamedSchema = sopDeclareNamedSchema
instance ToColumns Smiley where
    type Columns Smiley = '[Int, Int]
    toColumns (Smiley d c) = [I d :* I c :* Nil]

data Smileys = Smileys
  { _smileysEntries  :: ![Smiley]
  , _smileysDate     :: !Day
  , _smileysUsername :: !Text
  , _smileysSmiley   :: !SmileyState
  } deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Smileys
deriveGeneric ''Smileys

instance NFData Smileys
instance ToJSON Smileys where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Smileys where parseJSON = sopParseJSON
instance ToSchema Smileys where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Reports
-------------------------------------------------------------------------------

type SmileysReport = Report
    "Smileys report"
    ReportGenerated
    (Vector :$ NP I '[Text, Text, Int, Day])

type SmileysReport' = Report
    "Smileys report"
    ReportGenerated
    (Vector :$ Smileys)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

makeLenses ''Res
makeLenses ''PostSmiley

deriveGeneric ''Res
deriveGeneric ''PostSmiley

instance ToJSON Res where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance ToJSON PostSmiley where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance FromJSON Res where
    parseJSON = sopParseJSON
instance FromJSON PostSmiley where
    parseJSON = sopParseJSON

instance ToSchema Res where
    declareNamedSchema = sopDeclareNamedSchema
instance ToSchema PostSmiley where
    declareNamedSchema = sopDeclareNamedSchema
