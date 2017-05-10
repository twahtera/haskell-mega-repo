{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
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
import Futurice.IsMaybe
import Data.Maybe (listToMaybe)
import Data.Csv (ToField (..))

import qualified FUM

import qualified Data.HashMap.Strict as HM

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
instance (ToJSON a, IsMaybe a) => ToJSON (ContactFD a) where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance ToSchema a => ToSchema (ContactFD a) where
    declareNamedSchema = sopDeclareNamedSchema
instance ToField (ContactFD a) where
    toField = toField . cfdNick

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
instance (ToJSON a, IsMaybe a) => ToJSON (ContactGH a) where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance ToSchema a => ToSchema (ContactGH a) where
    declareNamedSchema = sopDeclareNamedSchema
instance ToField (ContactGH a) where
    toField = toField . cghNick

data Contact avatar = Contact
    { contactLogin      :: !FUM.UserName
    , contactFirst      :: !Text
    , contactName       :: !Text
    , contactEmail      :: !Text
    , contactPhones     :: ![Text]
    , contactTitle      :: !(Maybe Text)
    , contactThumb      :: !avatar
    , contactImage      :: !Text
    , contactFlowdock   :: !(Tri (ContactFD avatar))
    , contactGithub     :: !(Tri (ContactGH avatar))
    , contactTeam       :: !(Maybe Text)
    , contactCompetence :: !(Maybe Text)
    , contactHrNumber   :: !(Maybe Text)
    }
  deriving
    ( Eq, Ord, Show, Read, Generic, Typeable
    , Functor, Foldable, Traversable
    )

instance NFData a => NFData (Contact a)

-- TH slices

deriveGeneric ''Contact
instance (ToJSON a, IsMaybe a) => ToJSON (Contact a) where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance ToSchema a => ToSchema (Contact a) where
    declareNamedSchema = sopDeclareNamedSchema
instance ToField a => ToNamedRecord (Contact a) where
    toNamedRecord Contact {..} = HM.fromList
        [ (,) "login"      $ toField contactLogin
        , (,) "first"      $ toField contactFirst
        , (,) "name"       $ toField contactName
        , (,) "email"      $ toField contactEmail
        , (,) "phones"     $ toField $ listToMaybe contactPhones
        , (,) "title"      $ toField contactTitle
        , (,) "thumb"      $ toField contactThumb
        , (,) "image"      $ toField contactImage
        , (,) "flowdock"   $ toField contactFlowdock
        , (,) "github"     $ toField contactGithub
        , (,) "team"       $ toField contactTeam
        , (,) "competence" $ toField contactCompetence
        , (,) "hrNumber"   $ toField contactHrNumber
        ]
instance DefaultOrdered (Contact a) where
    headerOrder = sopHeaderOrder
