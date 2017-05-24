{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.Status where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation (ToHtml (..))
import Data.Aeson.Compat (Value (String), withText)
import Data.Swagger
       (SwaggerType (SwaggerString), ToParamSchema (..), enum_, type_)
import Futurice.Generics
import Servant           (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Map  as Map
import qualified Data.Text as T

-- | User status
data Status
    = StatusActive
    | StatusSuspended  -- ^ temporary status.
    | StatusDeleted
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''Status
deriveGeneric ''Status

instance NFData Status

instance Arbitrary Status where
    arbitrary = sopArbitrary
    shrink    = sopShrink

statusToText :: Status -> Text
statusToText StatusActive    = "active"
statusToText StatusSuspended = "suspended"
statusToText StatusDeleted   = "deleted"

statusFromText :: Text -> Maybe Status
statusFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ statusToText x, x)) [minBound .. maxBound]

_Status :: Prism' Text Status
_Status = prism' statusToText statusFromText

instance ToHtml Status where
    toHtmlRaw = toHtml
    -- TODO:
    toHtml = toHtml . statusToText

instance ToParamSchema Status where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & enum_ ?~ (String . statusToText <$> [minBound .. maxBound])

instance ToJSON Status where
    toJSON = String . statusToText

instance FromJSON Status where
    parseJSON = withText "Status" $ \t ->
        maybe (fail $ "invalid status " <> t ^. unpacked) pure $ t ^? _Status

instance FromHttpApiData Status where
    parseUrlPiece t =
        maybe (Left $ "invalid status " <> t) Right $ t ^? _Status

instance ToHttpApiData Status where
    toUrlPiece = statusToText
