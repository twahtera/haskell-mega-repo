{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | /TODO/ merge into 'Futurice.Generics'
module Futurice.Generics.Enum (
    IsEnumType,
    EnumInstances (..),
    sopEnumInstances,
    ) where

import Futurice.Prelude
import Prelude ()

import qualified Data.Aeson.Compat    as Aeson
import qualified Data.Map             as Map
import qualified Data.Swagger         as Swagger
import qualified Data.Swagger.Declare as Swagger
import qualified Data.Text            as T
import qualified Generics.SOP         as SOP

type IsEnumType (a :: *) = (SOP.Generic a, SOP.All ((~) '[]) (SOP.Code a))

-- | A record bundling many instance implementations:
--
data EnumInstances a = EnumInstances
    { enumToText             :: a -> Text
    , enumFromText           :: Text -> Maybe a
    , enumPrism              :: Prism' Text a
    -- universe
    , enumUniverse           :: [a]
    -- aeson
    , enumToJSON             :: a -> Value
    , enumToEncoding         :: a -> Aeson.Encoding
    , enumParseJSON          :: Value -> Aeson.Parser a
    -- swagger2
    , enumToParamSchema      :: forall proxy t. proxy a -> Swagger.ParamSchema t
    , enumDeclareNamedSchema :: forall proxy. proxy a -> Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema
    -- http-api-data
    , enumToUrlPiece         :: a -> Text
    , enumParseUrlPiece      :: Text -> Either Text a
    }

-- | Create 'EnumInstances" for a generic type.
--
-- /TODO:/ remove Enum a. Bounded a constraints
--
sopEnumInstances
    :: forall a. (IsEnumType a, Enum a, Bounded a, SOP.HasDatatypeInfo a)
    => NP (K Text) (SOP.Code a)     -- ^ json & url param etc. constructor names.
    -> EnumInstances a 
sopEnumInstances names = EnumInstances
    { enumToText             = enumToText_
    , enumFromText           = enumFromText_
    , enumPrism              = prism' enumToText_ enumFromText_
    , enumUniverse           = enumUniverse_
    , enumToJSON             = enumToJSON_
    , enumToEncoding         = enumToEncoding_
    , enumParseJSON          = enumParseJSON_
    , enumToParamSchema      = enumToParamSchema_
    , enumDeclareNamedSchema = enumDeclareNamedSchema_
    , enumToUrlPiece         = enumToText_
    , enumParseUrlPiece      = enumFromTextE_
    }
  where
    name :: String
    name = SOP.datatypeName (SOP.datatypeInfo (Proxy :: Proxy a))

    nameT :: Text
    nameT = name ^. packed

    enumUniverse_ :: [a]
    enumUniverse_ = [minBound .. maxBound]

    -- TODO: use text-trie
    namesM :: Map Text a
    namesM = Map.fromList $ map
        (\x -> (T.toLower $ enumToText_ x, x))
        enumUniverse_

    enumToText_ :: a -> Text
    enumToText_ x = SOP.hcollapse $
        SOP.hliftA2 (\(K n) _ -> K n) names (SOP.unSOP (SOP.from x))

    enumFromTextE_ :: Text -> Either Text a
    enumFromTextE_ t = maybe
        (Left $ "Invalid " <> nameT <> ": " <> t)
        Right
        (enumFromText_ t)

    enumFromText_ :: Text -> Maybe a
    enumFromText_ t = namesM ^? ix (T.toLower t)

    enumToJSON_ :: a -> Value
    enumToJSON_ = Aeson.String . enumToText_

    enumToEncoding_ :: a -> Aeson.Encoding
    enumToEncoding_ = Aeson.toEncoding . enumToText_

    enumParseJSON_ :: Value -> Aeson.Parser a
    enumParseJSON_ = Aeson.withText name $ \t ->
        either (fail . view unpacked) pure (enumFromTextE_ t)

    enumToParamSchema_ :: forall proxy t. proxy a -> Swagger.ParamSchema t
    enumToParamSchema_ _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

    enumDeclareNamedSchema_
        :: forall proxy. proxy a
        -> Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema
    enumDeclareNamedSchema_ _ = pure $ Swagger.NamedSchema (Just nameT) $ mempty
        & Swagger.paramSchema .~ enumToParamSchema_ Proxy
