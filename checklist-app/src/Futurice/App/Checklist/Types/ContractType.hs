{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.ContractType where

import Data.Aeson.Compat (Value (String), withText)
import Data.Swagger      (SwaggerType (SwaggerString), enum_, type_)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Map  as Map
import qualified Data.Text as T

-- | Contract type affect what's need to be done.
data ContractType
    = ContractTypePermanent
    | ContractTypeExternal
    | ContractTypeFixedTerm
    | ContractTypePartTimer
    | ContractTypeSummerWorker
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''ContractType
deriveGeneric ''ContractType

instance NFData ContractType

instance Arbitrary ContractType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

contractTypeToText :: ContractType -> Text
contractTypeToText ContractTypePermanent    = "permanent"
contractTypeToText ContractTypeExternal     = "external"
contractTypeToText ContractTypeFixedTerm    = "fixed-term"
contractTypeToText ContractTypePartTimer    = "part-timer"
contractTypeToText ContractTypeSummerWorker = "summer-worker"

contractTypeFromText :: Text -> Maybe ContractType
contractTypeFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ contractTypeToText x, x)) [minBound .. maxBound]

_ContractType :: Prism' Text ContractType
_ContractType = prism' contractTypeToText contractTypeFromText

instance ToParamSchema ContractType where
    toParamSchema _ = mempty
          & type_ .~ SwaggerString
          & enum_ ?~ (String . contractTypeToText <$> [minBound .. maxBound])

instance ToJSON ContractType where
    toJSON = String . contractTypeToText

instance FromJSON ContractType where
    parseJSON = withText "ContractType" $ \t ->
        maybe (fail $ "invalid contractType " <> t ^. unpacked) pure $ t ^? _ContractType

instance FromHttpApiData ContractType where
    parseUrlPiece t =
        maybe (Left $ "invalid contractType " <> t) Right $ t ^? _ContractType

instance ToHttpApiData ContractType where
    toUrlPiece = contractTypeToText
