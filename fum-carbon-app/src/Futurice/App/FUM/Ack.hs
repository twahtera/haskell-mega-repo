{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Ack (
    Ack (..),
    ) where


import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat (object, (.=))
import Data.Swagger      (NamedSchema (..))
import Futurice.Generics

-- | 'Futurice.App.FUM.Command.Command' response: 'Ack'-nowledgement.
data Ack
    = AckOk         -- ^ No follow-up actions.
    | AckErr Text   -- ^ Some error.
    | AckLoad Text  -- ^ Load page. If nothing reload current.
  deriving (Eq, Show)

ackObject :: Text -> [AesonPair] -> Value
ackObject a ps = object $ "ack" .= a : ps

instance ToJSON Ack where
    toJSON AckOk         = ackObject "ok" []
    toJSON (AckErr err)  = ackObject "error" $
        [ "reason" .= err
        ]
    toJSON (AckLoad url) = ackObject "load" $
        [ "url" .= url
        ]

instance ToSchema Ack where
    declareNamedSchema _ = pure $ NamedSchema (Just "Ack") mempty

instance Semigroup Ack where
    AckOk <> x = x
    x <> AckOk = x
    x@AckErr {} <> _ = x
    _ <> x@AckErr {} = x
    AckLoad u <> AckLoad _ = AckLoad u -- first url wins

instance Monoid Ack where
    mempty = AckOk
    mappend = (<>)
