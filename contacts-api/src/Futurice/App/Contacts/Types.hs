{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.Contacts.Types (
    Tri(..),
    ContactFD(..),
    ContactGH(..),
    Contact(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.App.Contacts.Types.Tri
import Futurice.Generics

import qualified FUM

data ContactFD avatar = ContactFD
    { cfdId     :: !Int       -- ^ Identifier
    , cfdNick   :: !Text      -- ^ Nick
    , cfdAvatar :: !avatar    -- ^ Avatar
    }
  deriving
    ( Eq, Ord, Show, Read, Generic, Typeable
    , Functor, Foldable, Traversable
    )

instance NFData a => NFData (ContactFD a)
deriveGeneric ''ContactFD
instance ToJSON a => ToJSON (ContactFD a) where toJSON = sopToJSON
instance ToSchema a => ToSchema (ContactFD a) where
    declareNamedSchema = sopDeclareNamedSchema

data ContactGH avatar = ContactGH
    { cghNick   :: !Text
    , cghAvatar :: !avatar
    }
  deriving
    ( Eq, Ord, Show, Read, Generic, Typeable
    , Functor, Foldable, Traversable
    )

instance NFData a => NFData (ContactGH a)
deriveGeneric ''ContactGH
instance ToJSON a => ToJSON (ContactGH a) where toJSON = sopToJSON
instance ToSchema a => ToSchema (ContactGH a) where
    declareNamedSchema = sopDeclareNamedSchema

data Contact avatar = Contact
    { contactLogin    :: !FUM.UserName
    , contactFirst    :: !Text
    , contactName     :: !Text
    , contactEmail    :: !Text
    , contactPhones   :: ![Text]
    , contactTitle    :: !(Maybe Text)
    , contactThumb    :: !avatar
    , contactImage    :: !Text
    , contactFlowdock :: !(Tri (ContactFD avatar))
    , contactGithub   :: !(Tri (ContactGH avatar))
    , contactTeam     :: !(Maybe Text)
    }
  deriving
    ( Eq, Ord, Show, Read, Generic, Typeable
    , Functor, Foldable, Traversable
    )

instance NFData a => NFData (Contact a)

-- TH slices

deriveGeneric ''Contact
instance ToJSON a => ToJSON (Contact a) where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance ToSchema a => ToSchema (Contact a) where
    declareNamedSchema = sopDeclareNamedSchema
