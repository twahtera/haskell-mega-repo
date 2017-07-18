{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.Status (
    Status (..),
    statusToText,
    statusFromText,
    -- * Prisms
    _Status,
    _StatusActive,
    _StatusSuspended,
    _StatusDeleted,
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Lucid.Foundation (ToHtml (..))
import Futurice.Prelude
import Prelude ()

-- | User status
data Status
    = StatusActive
    | StatusSuspended  -- ^ temporary status.
    | StatusDeleted
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''Status
deriveGeneric ''Status

ei :: EnumInstances Status
ei = sopEnumInstances $
    K "active" :*
    K "suspended" :*
    K "deleted" :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

statusToText :: Status -> Text
statusToText = enumToText ei

statusFromText :: Text -> Maybe Status
statusFromText = enumFromText ei

_Status :: Prism' Text Status
_Status = enumPrism ei

instance NFData Status

instance Arbitrary Status where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToHtml Status where
    toHtmlRaw = toHtml
    toHtml = toHtml . enumToText ei

instance ToParamSchema Status where
    toParamSchema = enumToParamSchema ei

instance ToSchema Status where
    declareNamedSchema = enumDeclareNamedSchema ei

instance ToJSON Status where
    toJSON = enumToJSON ei

instance FromJSON Status where
    parseJSON = enumParseJSON ei

instance FromHttpApiData Status where
    parseUrlPiece = enumParseUrlPiece ei

instance ToHttpApiData Status where
    toUrlPiece = enumToUrlPiece ei
