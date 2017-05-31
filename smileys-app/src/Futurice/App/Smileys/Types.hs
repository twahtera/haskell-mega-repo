{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Smileys.Types (
    PostSmiley(..),
    Res(..),
    Smileys(..)
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))

import Data.Aeson.Compat
       (AesonException (..), eitherDecode, encode)

-------------------------------------------------------------------------------
-- Smiley
-------------------------------------------------------------------------------

type HourEntries = [HourEntry]

data PostSmiley = PostSmiley
  { _postSmileyEntries :: !HourEntries
  , _postSmileyDate :: !Day
  , _postSmileySmiley   :: !SmileyValue
  } deriving (Eq, Ord, Show, Typeable, Generic)

data Res = Res
    { _resStatus :: !Text
    } deriving (Show)

type ProjectId = Int
type TaskId = Int
type SmileyValue = Int

data HourEntry = HourEntry
  { _smileyProject :: !ProjectId
  , _smileyTask    :: !TaskId
  } deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''HourEntry
deriveGeneric ''HourEntry

instance NFData HourEntry
instance ToJSON HourEntry where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON HourEntry where parseJSON = sopParseJSON
instance ToSchema HourEntry where declareNamedSchema = sopDeclareNamedSchema

instance FromField HourEntries where
  fromField = Postgres.fromJSONField
instance ToField HourEntries where
  toField = Postgres.toJSONField

data Smileys = Smileys
  { _smileysEntries  :: !HourEntries
  , _smileysUsername :: !Text
  , _smileysSmiley   :: !SmileyValue
  , _smileysDate     :: !Day
  } deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Smileys
deriveGeneric ''Smileys

instance NFData Smileys
instance ToJSON Smileys where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Smileys where parseJSON = sopParseJSON
instance ToSchema Smileys where declareNamedSchema = sopDeclareNamedSchema
instance FromRow Smileys where
  fromRow = Smileys <$> field <*> field <*> field <*> field

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
