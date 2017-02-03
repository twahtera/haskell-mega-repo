{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.Location where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat (Value (String), withText)
import Data.Swagger
       (SwaggerType (SwaggerString), ToParamSchema (..), enum_, type_)
import Futurice.Generics
import Servant           (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Map  as Map
import qualified Data.Text as T

-- | Tasks can be location specific.
data Location
    = LocHelsinki
    | LocTampere
    | LocBerlin
    | LocLondon
    | LocStockholm
    | LocMunich
    | LocOther
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''Location
deriveGeneric ''Location

instance Arbitrary Location where
    arbitrary = sopArbitrary
    shrink    = sopShrink

locationToText :: Location -> Text
locationToText LocHelsinki  = "Helsinki"
locationToText LocTampere   = "Tampere"
locationToText LocBerlin    = "Berlin"
locationToText LocLondon    = "London"
locationToText LocStockholm = "Stockholm"
locationToText LocMunich    = "Munich"
locationToText LocOther     = "Other"

locationFromText :: Text -> Maybe Location
locationFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ locationToText x, x)) [minBound .. maxBound]

_Location :: Prism' Text Location
_Location = prism' locationToText locationFromText

instance ToParamSchema Location where
    toParamSchema _ = mempty
          & type_ .~ SwaggerString
          & enum_ ?~ (String . locationToText <$> [minBound .. maxBound])

instance ToJSON Location where
    toJSON = String . locationToText

instance FromJSON Location where
    parseJSON = withText "Location" $ \t ->
        maybe (fail $ "invalid location " <> t ^. unpacked) pure $ t ^? _Location

instance FromHttpApiData Location where
    parseUrlPiece t =
        maybe (Left $ "invalid location " <> t) Right $ t ^? _Location

instance ToHttpApiData Location where
    toUrlPiece = locationToText
