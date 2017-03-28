{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.EmailProxy.Types where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics

-- | TODO, what we want to return?
data Res = Res
    { _resStatus  :: !Text
    , _resMessage :: !Text
    }
  deriving (Show)

type EmailAddress = Text

data Req = Req
    { _reqTo      :: !(NonEmpty EmailAddress)
    , _reqBcc     :: !(Maybe (NonEmpty EmailAddress)) -- maybe to make generic derivation work as we want it to.
    , _reqFrom    :: !EmailAddress
    , _reqReplyTo :: !(Maybe EmailAddress)
    , _reqSubject :: !Text
    , _reqBody    :: !Text
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
