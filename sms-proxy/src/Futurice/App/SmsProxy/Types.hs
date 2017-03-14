{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.SmsProxy.Types where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

-- | TODO, what we want to return?
data Res = Res
    { _resTo     :: !Text
    , _resStatus :: !Text
    }
  deriving (Show)

data Req = Req
    { _reqTo   :: !Text
    , _reqText :: !Text
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

makeLenses ''Res
makeLenses ''Req

deriveGeneric ''Res
deriveGeneric ''Req

instance ToJSON Res where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance ToJSON Req where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance FromJSON Res where
    parseJSON = sopParseJSON
instance FromJSON Req where
    parseJSON = sopParseJSON

instance ToSchema Res where
    declareNamedSchema = sopDeclareNamedSchema
instance ToSchema Req where
    declareNamedSchema = sopDeclareNamedSchema
