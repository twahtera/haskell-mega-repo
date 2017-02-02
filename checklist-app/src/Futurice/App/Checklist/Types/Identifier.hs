{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Futurice.App.Checklist.Types.Identifier (
    Identifier (..),
    identifierToText,
    Entity(..),
    HasIdentifier (..),
    identifierText,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens      (Getter, to)
import Data.Swagger
       (SwaggerType (SwaggerString), ToParamSchema (..), format, type_)
import Futurice.Generics
import Servant.API

import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

newtype Identifier a = Identifier UUID
    deriving (Eq, Ord, Show, Typeable, Generic)

identifierToText :: Identifier a -> Text
identifierToText (Identifier u) = UUID.toText u

instance Arbitrary (Identifier a) where
    arbitrary = Identifier <$> arbitrary

instance HasUUID (Identifier a) where
    uuid = lens (\(Identifier u) -> u) (\_ u -> Identifier u)

instance ToHttpApiData (Identifier a) where
    toUrlPiece   = toUrlPiece . view uuid
    toQueryParam = toQueryParam . view uuid

instance FromHttpApiData (Identifier a) where
    parseUrlPiece   = fmap Identifier . parseUrlPiece
    parseQueryParam = fmap Identifier . parseQueryParam

instance Entity a => ToParamSchema (Identifier a) where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "Identifier " <> entityName (Proxy :: Proxy a) <> ": uuid"

instance ToJSON (Identifier a) where
    toJSON (Identifier u) = toJSON u

instance FromJSON (Identifier a) where
    parseJSON = fmap Identifier . parseJSON

instance Postgres.ToField (Identifier a) where
    toField (Identifier i) = Postgres.toField i

-------------------------------------------------------------------------------
-- HasIdentifier
-------------------------------------------------------------------------------

-- | TODO: evaluate if we really need this, we have 'Key'
class Entity ident => HasIdentifier entity ident | entity -> ident where
    identifier :: Lens' entity (Identifier ident)

identifierText :: HasIdentifier entity ident => Getter entity Text
identifierText = identifier . to identifierToText

instance Entity e => HasIdentifier (Identifier e) e where
    identifier = id

-------------------------------------------------------------------------------
-- Entity
-------------------------------------------------------------------------------

-- | Class of entities.
class Entity a where
    entityName :: Proxy a -> Text
