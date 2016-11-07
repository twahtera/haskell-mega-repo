{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Futurice.App.Checklist.Types.Identifier (
    Identifier (..),
    identifierToText,
    Entity(..),
    HasIdentifier (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Swagger
       (SwaggerType (SwaggerString), ToParamSchema (..), format, type_)
import Futurice.Generics
import Servant.API

import qualified Data.UUID as UUID

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

-------------------------------------------------------------------------------
-- HasIdentifier
-------------------------------------------------------------------------------

-- | TODO: evaluate if we really need this
class Entity ident => HasIdentifier entity ident | entity -> ident where
    identifier :: Lens' entity (Identifier ident)

instance Entity e => HasIdentifier (Identifier e) e where
    identifier = id

-------------------------------------------------------------------------------
-- Entity
-------------------------------------------------------------------------------

-- | Class of entities.
class Entity a where
    entityName :: Proxy a -> Text
