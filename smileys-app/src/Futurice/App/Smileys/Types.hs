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

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified FUM

import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField   (ToField (..), toJSONField)
import Database.PostgreSQL.Simple.ToRow     (ToRow (..))

-------------------------------------------------------------------------------
-- Smiley
-------------------------------------------------------------------------------

type ProjectId = Int
type TaskId = Int
type SmileyValue = Int

newtype HourEntries = HourEntries { getHourEntries :: [HourEntry] }
  deriving (Eq, Ord, Show, Typeable, Generic)

data PostSmiley = PostSmiley
    { _postSmileyEntries :: !HourEntries
    , _postSmileyDate    :: !Day
    , _postSmileySmiley  :: !SmileyValue
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

data Res = Res
    { _resStatus :: !Text
    }
  deriving (Show)


data HourEntry = HourEntry
    { _smileyProject :: !ProjectId
    , _smileyTask    :: !TaskId
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

data Smileys = Smileys
    { _smileysEntries  :: !HourEntries
    , _smileysUsername :: !FUM.UserName
    , _smileysSmiley   :: !SmileyValue
    , _smileysDate     :: !Day
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

deriveGeneric ''HourEntries
deriveGeneric ''Smileys
deriveGeneric ''HourEntry
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

instance NFData HourEntry
instance ToJSON HourEntry where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON HourEntry where parseJSON = sopParseJSON
instance ToSchema HourEntry where declareNamedSchema = sopDeclareNamedSchema

instance NFData HourEntries
instance FromJSON HourEntries where
    parseJSON = fmap HourEntries . parseJSON 
instance ToJSON HourEntries where
    toJSON = toJSON . getHourEntries
    toEncoding = toEncoding . getHourEntries
instance ToSchema HourEntries where
    declareNamedSchema = newtypeDeclareNamedSchema
instance FromField HourEntries where
    fromField = Postgres.fromJSONField
instance ToField HourEntries where
    toField = toJSONField

instance NFData Smileys
instance ToJSON Smileys where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Smileys where parseJSON = sopParseJSON
instance ToSchema Smileys where declareNamedSchema = sopDeclareNamedSchema
instance FromRow Smileys where
    fromRow = Smileys <$> field <*> field <*> field <*> field
instance ToRow Smileys where
    toRow (Smileys a b c d) = toRow (a, b, c, d)
